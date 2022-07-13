-----------------------------------------------------------------------
--  awa-permissions-services -- Permissions controller
--  Copyright (C) 2011, 2012, 2013, 2017, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with ADO.Queries;
with ADO.Sessions.Entities;
with ADO.Statements;
with ADO.Caches.Discrete;
with Util.Log.Loggers;
with Util.Strings;

with Security.Policies.URLs;

with AWA.Permissions.Models;
package body AWA.Permissions.Services is

   use ADO.Sessions;
   use type ADO.Identifier;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Permissions.Services");

   package Permission_Cache is
     new ADO.Caches.Discrete (Element_Type => Integer);

   --  ------------------------------
   --  Check if the permission with the name <tt>Name</tt> is granted for the current user.
   --  If the <tt>Entity</tt> is defined, an <tt>Entity_Permission</tt> is created and verified.
   --  Returns True if the user is granted the given permission.
   --  ------------------------------
   function Has_Permission (Name   : in Util.Beans.Objects.Object;
                            Entity : in Util.Beans.Objects.Object)
                            return Util.Beans.Objects.Object is
      use type Security.Contexts.Security_Context_Access;

      Context : constant Security.Contexts.Security_Context_Access := Security.Contexts.Current;
      Perm    : constant String := Util.Beans.Objects.To_String (Name);
      Result  : Boolean;
   begin
      if Util.Beans.Objects.Is_Empty (Name) or else Context = null then
         Log.Error ("No security context: permission {0} is refused", Perm);
         Result := False;

      elsif Util.Beans.Objects.Is_Empty (Entity) then
         Result := Context.Has_Permission (Perm);

      else
         declare
            P : Entity_Permission (Security.Permissions.Get_Permission_Index (Perm));
         begin
            P.Entity := ADO.Identifier (Util.Beans.Objects.To_Long_Long_Integer (Entity));
            Result := Context.Has_Permission (P);
         end;
      end if;
      return Util.Beans.Objects.To_Object (Result);

   exception
      when Security.Permissions.Invalid_Name =>
         Log.Error ("Invalid permission {0}", Perm);
         raise;
   end Has_Permission;

   URI        : aliased constant String := "http://code.google.com/p/ada-awa/auth";

   --  ------------------------------
   --  Register the security EL functions in the EL mapper.
   --  ------------------------------
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
   begin
      Mapper.Set_Function (Name      => "hasPermission",
                           Namespace => URI,
                           Func      => Has_Permission'Access,
                           Optimize  => False);
   end Set_Functions;

   --  ------------------------------
   --  Get the permission manager associated with the security context.
   --  Returns null if there is none.
   --  ------------------------------
   function Get_Permission_Manager (Context : in Security.Contexts.Security_Context'Class)
                                    return Permission_Manager_Access is
      use type Security.Policies.Policy_Manager_Access;

      M : constant Security.Policies.Policy_Manager_Access
        := Context.Get_Permission_Manager;
   begin
      if M = null then
         Log.Info ("There is no permission manager");
         return null;

      elsif not (M.all in Permission_Manager'Class) then
         Log.Info ("Permission manager is not a AWA permission manager");
         return null;

      else
         return Permission_Manager'Class (M.all)'Access;
      end if;
   end Get_Permission_Manager;

   --  ------------------------------
   --  Get the permission manager associated with the security context.
   --  Returns null if there is none.
   --  ------------------------------
   function Get_Permission_Manager (Context : in ASC.Service_Context_Access)
                                    return Permission_Manager_Access is
      Manager : constant Security.Policies.Policy_Manager_Access
        := Context.Get_Application.Get_Security_Manager;
   begin
      return Permission_Manager'Class (Manager.all)'Access;
   end Get_Permission_Manager;

   --  ------------------------------
   --  Get the application instance.
   --  ------------------------------
   function Get_Application (Manager : in Permission_Manager)
                             return AWA.Applications.Application_Access is
   begin
      return Manager.App;
   end Get_Application;

   --  ------------------------------
   --  Set the application instance.
   --  ------------------------------
   procedure Set_Application (Manager : in out Permission_Manager;
                              App : in AWA.Applications.Application_Access) is
   begin
      Manager.App := App;
   end Set_Application;

   --  ------------------------------
   --  Initialize the permissions.
   --  ------------------------------
   procedure Start (Manager : in out Permission_Manager) is
      package Perm renames Security.Permissions;

      DB         : ADO.Sessions.Master_Session := Manager.App.Get_Master_Session;
      Cache      : constant Permission_Cache.Cache_Type_Access := new Permission_Cache.Cache_Type;
      Count      : constant Perm.Permission_Index := Perm.Get_Last_Permission_Index;
      Last       : ADO.Identifier := 0;
      Insert     : ADO.Statements.Insert_Statement;
      Stmt       : ADO.Statements.Query_Statement;
      Load_Count : Natural := 0;
      Add_Count  : Natural := 0;
   begin
      Log.Info ("Initializing {0} permissions", Perm.Permission_Index'Image (Count));

      DB.Begin_Transaction;

      --  Step 1: load the permissions from the database.
      Stmt := DB.Create_Statement ("SELECT id, name FROM awa_permission");
      Stmt.Execute;
      while Stmt.Has_Elements loop
         declare
            Id   : constant Integer := Stmt.Get_Integer (0);
            Name : constant String := Stmt.Get_String (1);
         begin
            Log.Debug ("Loaded permission {0} as {1}", Name, Util.Strings.Image (Id));
            Permission_Cache.Insert (Cache.all, Name, Id);
            Load_Count := Load_Count + 1;
            if ADO.Identifier (Id) > Last then
               Last := ADO.Identifier (Id);
            end if;
         end;
         Stmt.Next;
      end loop;

      --  Step 2: Check that every application permission is defined in the database.
      --  Create the new entries and allocate them the last id.
      for I in 1 .. Count loop
         declare
            Name   : constant String := Perm.Get_Name (I);
            Result : Integer;
         begin
            Manager.Map (I) := ADO.Identifier (Cache.Find (Name));

         exception
            when ADO.Caches.No_Value =>
               Last := Last + 1;
               Log.Info ("Adding permission {0} as {1}", Name, ADO.Identifier'Image (Last));
               Insert := DB.Create_Statement (AWA.Permissions.Models.PERMISSION_TABLE);
               Insert.Save_Field (Name => "id", Value => Last);
               Insert.Save_Field (Name => "name", Value => Name);
               Insert.Execute (Result);
               Cache.Insert (Name, Integer (Last));
               Manager.Map (I) := Last;
               Add_Count := Add_Count + 1;

         end;
      end loop;
      DB.Add_Cache ("permission", Cache.all'Access);
      DB.Commit;
      if Add_Count > 0 then
         Log.Info ("Found {0} permissions in the database and created {1} permissions",
                   Util.Strings.Image (Load_Count), Util.Strings.Image (Add_Count));
      else
         Log.Info ("Found {0} permissions in the database",
                   Util.Strings.Image (Load_Count));
      end if;
   end Start;

   --  ------------------------------
   --  Add a permission for the current user to access the entity identified by
   --  <b>Entity</b> and <b>Kind</b>.
   --  ------------------------------
   procedure Add_Permission (Manager    : in Permission_Manager;
                             Entity     : in ADO.Identifier;
                             Kind       : in ADO.Entity_Type;
                             Workspace  : in ADO.Identifier;
                             Permission : in Security.Permissions.Permission_Index) is
      Ctx  : constant AWA.Services.Contexts.Service_Context_Access
        := AWA.Services.Contexts.Current;
      DB   : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Perm : AWA.Permissions.Models.ACL_Ref;
   begin
      Log.Info ("Adding permission");

      Ctx.Start;
      Perm.Set_Entity_Type (Kind);
      Perm.Set_User_Id (Ctx.Get_User_Identifier);
      Perm.Set_Entity_Id (Entity);
      Perm.Set_Writeable (False);
      Perm.Set_Workspace_Id (Workspace);
      Perm.Set_Permission (Manager.Map (Permission));
      Perm.Save (DB);
      Ctx.Commit;
   end Add_Permission;

   --  ------------------------------
   --  Check that the current user has the specified permission.
   --  Raise NO_PERMISSION exception if the user does not have the permission.
   --  ------------------------------
   procedure Check_Permission (Manager    : in Permission_Manager;
                               Entity     : in ADO.Identifier;
                               Kind       : in ADO.Entity_Type;
                               Permission : in Permission_Type) is
      pragma Unreferenced (Manager, Permission);
      use AWA.Services;

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : constant Session := AWA.Services.Contexts.Get_Session (Ctx);
      Query : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Permissions.Models.Query_Check_Entity_Permission);
      Query.Bind_Param ("user_id", User);
      Query.Bind_Param ("entity_id", Entity);
      Query.Bind_Param ("entity_type", Integer (Kind));
      declare
         Stmt : ADO.Statements.Query_Statement := DB.Create_Statement (Query);
      begin
         Stmt.Execute;
         if not Stmt.Has_Elements then
            Log.Info ("User {0} does not have permission to access entity {1}/{2}",
                      ADO.Identifier'Image (User), ADO.Identifier'Image (Entity),
                      ADO.Entity_Type'Image (Kind));
            raise NO_PERMISSION;
         end if;
      end;
   end Check_Permission;

   --  ------------------------------
   --  Get the role names that grant the given permission.
   --  ------------------------------
   function Get_Role_Names (Manager : in Permission_Manager;
                            Permission : in Security.Permissions.Permission_Index)
                            return Security.Policies.Roles.Role_Name_Array is
   begin
      return Manager.Roles.Get_Role_Names (Manager.Roles.Get_Grants (Permission));
   end Get_Role_Names;

   --  ------------------------------
   --  Add a permission for the user <b>User</b> to access the entity identified by
   --  <b>Entity</b> which is of type <b>Kind</b>.
   --  ------------------------------
   procedure Add_Permission (Manager    : in Permission_Manager;
                             Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Identifier;
                             Kind       : in ADO.Entity_Type;
                             Workspace  : in ADO.Identifier;
                             Permission : in Security.Permissions.Permission_Index) is
      Acl : AWA.Permissions.Models.ACL_Ref;
   begin
      Acl.Set_User_Id (User);
      Acl.Set_Entity_Type (Kind);
      Acl.Set_Entity_Id (Entity);
      Acl.Set_Writeable (False);
      Acl.Set_Workspace_Id (Workspace);
      Acl.Set_Permission (Manager.Map (Permission));
      Acl.Save (Session);

      Log.Info ("Permission created for {0} to access {1}, entity type {2}",
                ADO.Identifier'Image (User),
                ADO.Identifier'Image (Entity),
                ADO.Entity_Type'Image (Kind));
   end Add_Permission;

   --  ------------------------------
   --  Add a permission for the user <b>User</b> to access the entity identified by
   --  <b>Entity</b>.
   --  ------------------------------
   procedure Add_Permission (Manager    : in Permission_Manager;
                             Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Objects.Object_Ref'Class;
                             Workspace  : in ADO.Identifier;
                             Permission : in Security.Permissions.Permission_Index) is
      Key  : constant ADO.Objects.Object_Key := Entity.Get_Key;
      Kind : constant ADO.Entity_Type
        := ADO.Sessions.Entities.Find_Entity_Type (Session => Session,
                                                   Object  => Key);
   begin
      Manager.Add_Permission (Session, User, ADO.Objects.Get_Value (Key),
                              Kind, Workspace, Permission);
   end Add_Permission;

   --  ------------------------------
   --  Create a permission manager for the given application.
   --  ------------------------------
   function Create_Permission_Manager (App : in AWA.Applications.Application_Access)
                                       return Security.Policies.Policy_Manager_Access is
      Result : constant AWA.Permissions.Services.Permission_Manager_Access
        := new AWA.Permissions.Services.Permission_Manager (10);
      RP : constant Security.Policies.Roles.Role_Policy_Access
        := new Security.Policies.Roles.Role_Policy;
      RU : constant Security.Policies.URLs.URL_Policy_Access
        := new Security.Policies.URLs.URL_Policy;
      RE : constant Entity_Policy_Access
        := new Entity_Policy;
   begin
      Result.Roles := RP;
      Result.Add_Policy (RP.all'Access);
      Result.Add_Policy (RU.all'Access);
      Result.Add_Policy (RE.all'Access);
      Result.Set_Application (App);

      Log.Info ("Creation of the AWA Permissions manager");
      return Result.all'Access;
   end Create_Permission_Manager;

   --  ------------------------------
   --  Delete all the permissions for a user and on the given workspace.
   --  ------------------------------
   procedure Delete_Permissions (Session   : in out ADO.Sessions.Master_Session;
                                 User      : in ADO.Identifier;
                                 Workspace : in ADO.Identifier) is
      Stmt   : ADO.Statements.Delete_Statement
        := Session.Create_Statement (Models.ACL_TABLE);
      Result : Natural;
   begin
      Stmt.Set_Filter (Filter => "workspace_id = ? AND user_id = ?");
      Stmt.Add_Param (Value => Workspace);
      Stmt.Add_Param (Value => User);
      Stmt.Execute (Result);
      Log.Info ("Deleted {0} permissions for user {1} in workspace {2}",
                Natural'Image (Result), ADO.Identifier'Image (User),
                ADO.Identifier'Image (Workspace));
   end Delete_Permissions;

end AWA.Permissions.Services;
