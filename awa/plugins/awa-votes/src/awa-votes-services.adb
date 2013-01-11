-----------------------------------------------------------------------
--  awa-votes-services -- Service Vote
--  Copyright (C) 2013 Stephane Carrez
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

with AWA.Permissions;
with AWA.Permissions.Services;
with AWA.Services.Contexts;
with AWA.Workspaces.Models;
with AWA.Workspaces.Modules;
with AWA.Votes.Models;

with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;

with Util.Log.Loggers;
package body AWA.Votes.Services is

   use AWA.Services;
   use ADO.Sessions;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Votes.Services");

   --  ------------------------------
   --  Set the permission to be used by the service.
   --  ------------------------------
   procedure Set_Permission (Service    : in out Vote_Service;
                             Permission : in Security.Permissions.Permission_Index) is
   begin
      Service.Vote_Permission := Permission;
   end Set_Permission;

   --  ------------------------------
   --  Vote for the given element.
   --  ------------------------------
   procedure Vote_For (Model  : in Vote_Service;
                       Object : in ADO.Objects.Object_Ref'Class;
                       Rating : in Integer) is
      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Log.Info ("Creating Vote");

      Ctx.Start;

      --  Check that the user has the vote permission on the given object.
      AWA.Permissions.Check (Permission => Model.Vote_Permission,
                             Entity     => Object);

      declare
         Stmt   : ADO.Statements.Insert_Statement
           := DB.Create_Statement (AWA.Votes.Models.VOTE_TABLE);
         Result : Integer;
      begin
         Stmt.Save_Field (Name  => "for_entity_id",
                          Value => Object.Get_Key);
         Stmt.Save_Field (Name  => "user_id",
                          Value => User);
         Stmt.Save_Field (Name  => "rating",
                          Value => Rating);
         Stmt.Save_Field (Name  => "for_entity_type",
                          Value => Object.Get_Key);
         Stmt.Execute (Result);
         if Result /= 1 then
            declare
               Update : ADO.Statements.Update_Statement
                 := DB.Create_Statement (AWA.Votes.Models.VOTE_TABLE);
            begin
               Update.Save_Field (Name => "rating", Value => Rating);
               Update.Set_Filter ("for_entity_id = ? and user_id = ? and for_entity_type = ?");
               Update.Add_Param (Object.Get_Key);
               Update.Add_Param (User);
               Update.Add_Param (Object.Get_Key);
               Update.Execute (Result);
            end;
         end if;
         Log.Info ("Insert result {0}", Integer'Image (Result));
      end;
      Ctx.Commit;
   end Vote_For;

end AWA.Votes.Services;
