-----------------------------------------------------------------------
--  awa-questions-services -- Service services
--  Copyright (C) 2012 Stephane Carrez
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
with Ada.Calendar;

with AWA.Permissions;
with AWA.Services.Contexts;
with AWA.Users.Models;
with AWA.Workspaces.Models;
with AWA.Workspaces.Modules;

with ADO.Sessions;

with Util.Log.Loggers;
package body AWA.Questions.Services is

   use AWA.Services;
   use ADO.Sessions;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Questions.Services");

   --  ------------------------------
   --  Create or save the question.
   --  ------------------------------
   procedure Save_Question (Model    : in out Question_Service;
                            Question : in out AWA.Questions.Models.Question_Ref'Class) is
      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      WS    : AWA.Workspaces.Models.Workspace_Ref;
   begin
      Ctx.Start;
      if Question.Is_Inserted then
         Log.Info ("Updating question {0}", ADO.Identifier'Image (Question.Get_Id));

         WS := AWA.Workspaces.Models.Workspace_Ref (Question.Get_Workspace);
      else
         Log.Info ("Creating new question {0}", String '(Question.Get_Title));

         AWA.Workspaces.Modules.Get_Workspace (DB, Ctx, WS);

         --  Check that the user has the create permission on the given workspace.
         AWA.Permissions.Check (Permission => ACL_Create_Questions.Permission,
                                Entity     => WS.Get_Id);
         Question.Set_Workspace (WS);
         Question.Set_Author (User);
      end if;

      if not Question.Is_Inserted then
         Question.Set_Create_Date (Ada.Calendar.Clock);
      else
         Question.Set_Edit_Date (Ada.Calendar.Clock);
      end if;
      Question.Save (DB);
      Ctx.Commit;
   end Save_Question;

end AWA.Questions.Services;
