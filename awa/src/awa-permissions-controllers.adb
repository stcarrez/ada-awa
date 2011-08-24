-----------------------------------------------------------------------
--  awa-permissions-controllers -- Permission controllers
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

with ADO.Sessions;
with ADO.Statements;

with Util.Log.Loggers;

with AWA.Applications;
with AWA.Users.Principals;
with AWA.Permissions.Services;
package body AWA.Permissions.Controllers is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Permissions.Controllers");

   --  ------------------------------
   --  Returns true if the user associated with the security context <b>Context</b> has
   --  the permission to access a database entity.  The security context contains some
   --  information about the entity to check and the permission controller will use an
   --  SQL statement to verify the permission.
   --  ------------------------------
   function Has_Permission (Handler : in Entity_Controller;
                            Context : in Security.Contexts.Security_Context'Class)
                            return Boolean is
      use AWA.Permissions.Services;
      use AWA.Users.Principals;
      use type ADO.Identifier;

      Manager : constant Permission_Manager_Access := Get_Permission_Manager (Context);
      User_Id : constant ADO.Identifier := Get_User_Identifier (Context.Get_User_Principal);
      Entity_Id : constant ADO.Identifier := Get_Context (Context, "entity_id");
   begin
      --  If there is no permission manager, permission is denied.
      if Manager = null or else User_Id = ADO.NO_IDENTIFIER then
         return False;
      end if;

      --  If the user is not logged, permission is denied.
      if Manager = null or else User_Id = ADO.NO_IDENTIFIER then
         Log.Info ("No user identifier in the security context.  Permission is denied");
         return False;
      end if;

      --  If the security context does not contain the entity identifier, permission is denied.
      if Entity_Id = ADO.NO_IDENTIFIER then
         Log.Info ("No entity identifier in the security context.  Permission is denied");
         return False;
      end if;

      --  Get a database session from the AWA application.
      --  (there is no guarantee that a AWA.Services.Contexts be available)
      declare
         App     : constant AWA.Applications.Application_Access := Manager.Get_Application;
         Session : constant ADO.Sessions.Session := App.Get_Session;
         Query   : ADO.Statements.Query_Statement := Session.Create_Statement (Handler.SQL);
         Result  : Integer;
      begin
         --  Build the query
         Query.Bind_Param (Name => "entity_type", Value => Handler.Entity);
         Query.Bind_Param (Name => "entity_id", Value => Entity_Id);
         Query.Bind_Param (Name => "user_id", Value => User_Id);

         --  Run the query.  We must get a single row result and the value must be > 0.
         Query.Execute;

         Result := Query.Get_Result_Integer;
         if Result >= 0 then
            Log.Info ("Permission granted to {0} on entity {1}",
                      ADO.Identifier'Image (User_Id),
                      ADO.Identifier'Image (Entity_Id));
            return True;
         else
            Log.Info ("Permission denied to {0} on entity {1}",
                      ADO.Identifier'Image (User_Id),
                      ADO.Identifier'Image (Entity_Id));
            return False;
         end if;
      end;
   end Has_Permission;

end AWA.Permissions.Controllers;
