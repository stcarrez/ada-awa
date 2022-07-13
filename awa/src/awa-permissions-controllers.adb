-----------------------------------------------------------------------
--  awa-permissions-controllers -- Permission controllers
--  Copyright (C) 2011, 2012, 2013, 2014, 2016, 2022 Stephane Carrez
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
with Util.Strings;

with AWA.Applications;
with AWA.Users.Principals;
with AWA.Permissions.Services;
with AWA.Services.Contexts;
package body AWA.Permissions.Controllers is

   Log : constant Util.Log.Loggers.Logger
     := Util.Log.Loggers.Create ("AWA.Permissions.Controllers");

   --  ------------------------------
   --  Returns true if the user associated with the security context <b>Context</b> has
   --  the permission to access a database entity.  The security context contains some
   --  information about the entity to check and the permission controller will use an
   --  SQL statement to verify the permission.
   --  ------------------------------
   overriding
   function Has_Permission (Handler : in Entity_Controller;
                            Context : in Security.Contexts.Security_Context'Class;
                            Permission : in Security.Permissions.Permission'Class)
                            return Boolean is
      use AWA.Permissions.Services;
      use AWA.Users.Principals;
      use type ADO.Identifier;
      use type ADO.Entity_Type;

      Manager   : constant Permission_Manager_Access := Get_Permission_Manager (Context);
      User_Id   : constant ADO.Identifier := Get_User_Identifier (Context.Get_User_Principal);
      Entity_Id : ADO.Identifier;
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

      if not (Permission in Entity_Permission'Class) then
         Log.Info ("Permission {0} denied because the entity is not given.",
                   Security.Permissions.Permission_Index'Image (Permission.Id));
         return False;
      end if;
      Entity_Id := Entity_Permission'Class (Permission).Entity;

      --  If the security context does not contain the entity identifier, permission is denied.
      if Entity_Id = ADO.NO_IDENTIFIER then
         Log.Info ("No entity identifier in the security context.  Permission is denied");
         return False;
      end if;

      declare
         function Get_Session return ADO.Sessions.Session;

         --  ------------------------------
         --  Get a database session from the AWA application.
         --  There is no guarantee that a AWA.Services.Contexts be available.
         --  But if we are within a service context, we must use the current session so
         --  that we are part of the current transaction.
         --  ------------------------------
         function Get_Session return ADO.Sessions.Session is
            package ASC renames AWA.Services.Contexts;
            use type ASC.Service_Context_Access;

            Ctx : constant ASC.Service_Context_Access := ASC.Current;
         begin
            if Ctx /= null then
               return AWA.Services.Contexts.Get_Session (Ctx);
            else
               return Manager.Get_Application.Get_Session;
            end if;
         end Get_Session;

         Session : constant ADO.Sessions.Session := Get_Session;
         Query   : ADO.Statements.Query_Statement := Session.Create_Statement (Handler.SQL);
         Result  : Integer;
      begin
         --  Build the query
         Query.Bind_Param (Name => "entity_id", Value => Entity_Id);
         Query.Bind_Param (Name => "user_id", Value => User_Id);
         if Handler.Entities (2) /= ADO.NO_ENTITY_TYPE then
            for I in Handler.Entities'Range loop
               exit when Handler.Entities (I) = ADO.NO_ENTITY_TYPE;
               Query.Bind_Param (Name  => "entity_type_" & Util.Strings.Image (I),
                                 Value => Handler.Entities (I));
            end loop;
         else
            Query.Bind_Param (Name => "entity_type", Value => Handler.Entities (1));
         end if;

         --  Run the query.  We must get a single row result and the value must be > 0.
         Query.Execute;

         Result := Query.Get_Result_Integer;
         if Result >= 0 and then Query.Has_Elements then
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
