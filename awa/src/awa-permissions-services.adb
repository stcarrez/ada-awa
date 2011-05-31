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

with AWA.Modules;
with AWA.Permissions.Models;
with AWA.Services.Contexts;

with ADO;
with ADO.Queries;
with ADO.Sessions;
with ADO.Statements;

with Util.Log.Loggers;
package body AWA.Permissions.Services is

   use Util.Log;
   use ADO.Sessions;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Permissions.Services");

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

end AWA.Permissions.Services;
