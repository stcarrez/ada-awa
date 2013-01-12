-----------------------------------------------------------------------
--  awa-votes-modules -- Module votes
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
with Util.Log.Loggers;

with Security.Permissions;

with AWA.Modules.Beans;
with AWA.Modules.Get;
with AWA.Votes.Beans;
with AWA.Permissions;
with AWA.Services.Contexts;
with AWA.Votes.Models;

with ADO.Sessions;
with ADO.Statements;
with ADO.Sessions.Entities;

package body AWA.Votes.Modules is

   use AWA.Services;
   use ADO.Sessions;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Votes.Module");

   package Register is new AWA.Modules.Beans (Module => Vote_Module,
                                              Module_Access => Vote_Module_Access);

   --  ------------------------------
   --  Initialize the votes module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Vote_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the votes module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Votes.Beans.Votes_Bean",
                         Handler => AWA.Votes.Beans.Create_Vote_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the votes module.
   --  ------------------------------
   function Get_Vote_Module return Vote_Module_Access is
      function Get is new AWA.Modules.Get (Vote_Module, Vote_Module_Access, NAME);
   begin
      return Get;
   end Get_Vote_Module;

   --  ------------------------------
   --  Vote for the given element.
   --  ------------------------------
   procedure Vote_For (Model       : in Vote_Module;
                       Id          : in ADO.Identifier;
                       Entity_Type : in String;
                       Permission  : in String;
                       Rating      : in Integer) is
      pragma Unreferenced (Model);

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : constant Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Kind  : ADO.Entity_Type;
   begin
      Log.Info ("User {0} votes for {1} rating {2}",
                ADO.Identifier'Image (User), Entity_Type & ADO.Identifier'Image (Id),
                Integer'Image (Rating));

      Ctx.Start;

      Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, Entity_Type);

      --  Check that the user has the vote permission on the given object.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Id);

      declare
         Stmt   : ADO.Statements.Insert_Statement
           := DB.Create_Statement (AWA.Votes.Models.VOTE_TABLE);
         Result : Integer;
      begin
         Stmt.Save_Field (Name  => "for_entity_id",
                          Value => Id);
         Stmt.Save_Field (Name  => "user_id",
                          Value => User);
         Stmt.Save_Field (Name  => "rating",
                          Value => Rating);
         Stmt.Save_Field (Name  => "for_entity_type",
                          Value => Kind);
         Stmt.Execute (Result);
         if Result /= 1 then
            declare
               Update : ADO.Statements.Update_Statement
                 := DB.Create_Statement (AWA.Votes.Models.VOTE_TABLE);
            begin
               Update.Save_Field (Name => "rating", Value => Rating);
               Update.Set_Filter ("for_entity_id = :id and user_id = :user "
                                  & "and for_entity_type = :type");
               Update.Bind_Param ("id", Id);
               Update.Bind_Param ("user", User);
               Update.Bind_Param ("type", Kind);
               Update.Execute (Result);
            end;
         end if;
      end;
      Ctx.Commit;
   end Vote_For;

end AWA.Votes.Modules;
