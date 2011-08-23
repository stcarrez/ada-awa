-----------------------------------------------------------------------
--  awa-permissions-services -- Permissions controller
--  Copyright (C) 2011 Stephane Carrez
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
with ADO.Sessions;
with ADO.Sessions.Entities;
with ADO.Statements;
with ADO.Model;

with Util.Log.Loggers;
with Util.Serialize.IO.XML;

with Security.Controllers.Roles;

with AWA.Permissions.Models;
with AWA.Services.Contexts;
--  with AWA.Permissions.Controllers;
package body AWA.Permissions.Services is

   use Util.Log;
   use ADO.Sessions;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Permissions.Services");

   --  ------------------------------
   --  Get the permission manager associated with the security context.
   --  Returns null if there is none.
   --  ------------------------------
   function Get_Permission_Manager (Context : in Security.Contexts.Security_Context'Class)
                                    return Permission_Manager_Access is
      use type Security.Permissions.Permission_Manager_Access;

      M : constant Security.Permissions.Permission_Manager_Access
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
                             Permission : in Permission_Type) is
      Ctx  : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB   : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Perm : AWA.Permissions.Models.ACL_Ref;
   begin
      Log.Info ("Adding permission");

      Ctx.Start;
      Perm.Set_Entity_Type (Kind);
      Perm.Set_User_Id (Ctx.Get_User_Identifier);
      Perm.Set_Entity_Id (Entity);
      Perm.Set_Writeable (Permission = AWA.Permissions.WRITE);
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
      Ctx   : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : constant Session := AWA.Services.Contexts.Get_Session (Ctx);
      Query : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Permissions.Models.Query_Check_Entity_Permission);
      Query.Bind_Param ("user_id", User);
      Query.Bind_Param ("entity_id", Entity);
      Query.Bind_Param ("entity_type", Integer (Kind));
      declare
         Stmt : ADO.Statements.Query_Statement:= DB.Create_Statement (Query);
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
   --  Read the policy file
   --  ------------------------------
   overriding
   procedure Read_Policy (Manager : in out Permission_Manager;
                          File    : in String) is

      use Util;

      Reader  : Util.Serialize.IO.XML.Parser;

      package Policy_Config is
        new Security.Permissions.Reader_Config (Reader, Manager'Unchecked_Access);
      package Role_Config is
        new Security.Controllers.Roles.Reader_Config (Reader, Manager'Unchecked_Access);
--        package Entity_Config is
--           new AWA.Permissions.Controllers.Reader_Config (Reader, Manager'Unchecked_Access);
   begin
      Log.Info ("Reading policy file {0}", File);

      Reader.Parse (File);

   end Read_Policy;

   --  ------------------------------
   --  Add a permission for the user <b>User</b> to access the entity identified by
   --  <b>Entity</b> which is of type <b>Kind</b>.
   --  ------------------------------
   procedure Add_Permission (Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Identifier;
                             Kind       : in ADO.Entity_Type;
                             Permission : in Permission_Type := READ) is
      Acl : AWA.Permissions.Models.ACL_Ref;
   begin
      Acl.Set_User_Id (User);
      Acl.Set_Entity_Type (Kind);
      Acl.Set_Entity_Id (Entity);
      Acl.Set_Writeable (Permission = WRITE);
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
                             Permission : in Permission_Type := READ) is
      Key  : constant ADO.Objects.Object_Key := Entity.Get_Key;
      Kind : constant ADO.Model.Entity_Type_Ref
        := ADO.Sessions.Entities.Find_Entity_Type (Session => Session,
                                                   Object  => Key);
   begin
      Add_Permission (Session, User, ADO.Objects.Get_Value (Key),
                      ADO.Entity_Type (Kind.Get_Id), Permission);
   end Add_Permission;

   --  ------------------------------
   --  Create a permission manager for the given application.
   --  ------------------------------
   function Create_Permission_Manager (App : in AWA.Applications.Application_Access)
                                       return Security.Permissions.Permission_Manager_Access is
      Result : constant AWA.Permissions.Services.Permission_Manager_Access
        := new AWA.Permissions.Services.Permission_Manager;
   begin
      Result.Set_Application (App);
      return Result.all'Access;
   end Create_Permission_Manager;

end AWA.Permissions.Services;
