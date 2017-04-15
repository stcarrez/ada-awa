-----------------------------------------------------------------------
--  awa-permissions-services -- Permissions controller
--  Copyright (C) 2011, 2012, 2013, 2017 Stephane Carrez
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
with Util.Log.Loggers;

with Security.Policies.Roles;
with Security.Policies.URLs;

with AWA.Permissions.Models;
with AWA.Services.Contexts;
package body AWA.Permissions.Services is

   use ADO.Sessions;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Permissions.Services");

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
      if Util.Beans.Objects.Is_Empty (Name) or Context = null then
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
   --  Add a permission for the current user to access the entity identified by
   --  <b>Entity</b> and <b>Kind</b>.
   --  ------------------------------
   procedure Add_Permission (Manager    : in Permission_Manager;
                             Entity     : in ADO.Identifier;
                             Kind       : in ADO.Entity_Type;
                             Workspace  : in ADO.Identifier;
                             Permission : in Permission_Type) is
      pragma Unreferenced (Manager);

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
      Perm.Set_Writeable (Permission = AWA.Permissions.WRITE);
      Perm.Set_Workspace_Id (Workspace);
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
   --  Add a permission for the user <b>User</b> to access the entity identified by
   --  <b>Entity</b> which is of type <b>Kind</b>.
   --  ------------------------------
   procedure Add_Permission (Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Identifier;
                             Kind       : in ADO.Entity_Type;
                             Workspace  : in ADO.Identifier;
                             Permission : in Permission_Type := READ) is
      Acl : AWA.Permissions.Models.ACL_Ref;
   begin
      Acl.Set_User_Id (User);
      Acl.Set_Entity_Type (Kind);
      Acl.Set_Entity_Id (Entity);
      Acl.Set_Writeable (Permission = WRITE);
      Acl.Set_Workspace_Id (Workspace);
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
   procedure Add_Permission (Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Objects.Object_Ref'Class;
                             Workspace  : in ADO.Identifier;
                             Permission : in Permission_Type := READ) is
      Key  : constant ADO.Objects.Object_Key := Entity.Get_Key;
      Kind : constant ADO.Entity_Type
        := ADO.Sessions.Entities.Find_Entity_Type (Session => Session,
                                                   Object  => Key);
   begin
      Add_Permission (Session, User, ADO.Objects.Get_Value (Key), Kind, Workspace, Permission);
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
      Result.Add_Policy (RP.all'Access);
      Result.Add_Policy (RU.all'Access);
      Result.Add_Policy (RE.all'Access);
      Result.Set_Application (App);

      Log.Info ("Creation of the AWA Permissions manager");
      return Result.all'Access;
   end Create_Permission_Manager;

end AWA.Permissions.Services;
