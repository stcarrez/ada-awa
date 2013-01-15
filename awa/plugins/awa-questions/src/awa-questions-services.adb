-----------------------------------------------------------------------
--  awa-questions-services -- Service services
--  Copyright (C) 2012, 2013 Stephane Carrez
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
with Ada.Characters.Conversions;

with AWA.Permissions;
with AWA.Services.Contexts;
with AWA.Users.Models;
with AWA.Workspaces.Models;
with AWA.Workspaces.Modules;
with AWA.Wikis.Parsers;
with AWA.Wikis.Writers;

with ADO.Sessions;
with ADO.Statements;

with Util.Log.Loggers;
package body AWA.Questions.Services is

   use AWA.Services;
   use ADO.Sessions;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Questions.Services");

   --  ------------------------------
   --  Create or save the question.
   --  ------------------------------
   procedure Save_Question (Model    : in Question_Service;
                            Question : in out AWA.Questions.Models.Question_Ref'Class) is
      pragma Unreferenced (Model);

      function To_Wide (Item : in String) return Wide_Wide_String
                        renames Ada.Characters.Conversions.To_Wide_Wide_String;

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
                                Entity     => WS);
         Question.Set_Workspace (WS);
         Question.Set_Author (User);
      end if;

      declare
         Text : constant String := AWA.Wikis.Writers.To_Text (To_Wide (Question.Get_Description),
                                                              AWA.Wikis.Parsers.SYNTAX_MIX);
         Last : Natural;
      begin
         if Text'Length < SHORT_DESCRIPTION_LENGTH then
            Last := Text'Last;
         else
            Last := SHORT_DESCRIPTION_LENGTH;
         end if;
         Question.Set_Short_Description (Text (Text'First .. Last) & "...");
      end;
      if not Question.Is_Inserted then
         Question.Set_Create_Date (Ada.Calendar.Clock);
      else
         Question.Set_Edit_Date (Ada.Calendar.Clock);
      end if;
      Question.Save (DB);
      Ctx.Commit;
   end Save_Question;

   --  ------------------------------
   --  Delete the question.
   --  ------------------------------
   procedure Delete_Question (Model    : in Question_Service;
                              Question : in out AWA.Questions.Models.Question_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;

      --  Check that the user has the delete permission on the given question.
      AWA.Permissions.Check (Permission => ACL_Delete_Questions.Permission,
                             Entity     => Question);

      --  Before deleting the question, delete the associated answers.
      declare
         Stmt : ADO.Statements.Delete_Statement
           := DB.Create_Statement (AWA.Questions.Models.ANSWER_TABLE);
      begin
         Stmt.Set_Filter (Filter => "question_id = ?");
         Stmt.Add_Param (Value => Question);
         Stmt.Execute;
      end;
      Question.Delete (DB);
      Ctx.Commit;
   end Delete_Question;

   --  ------------------------------
   --  Load the question.
   --  ------------------------------
   procedure Load_Question (Model    : in Question_Service;
                            Question : in out AWA.Questions.Models.Question_Ref'Class;
                            Id       : in ADO.Identifier) is
      pragma Unreferenced (Model);

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Found : Boolean;
   begin
      Question.Load (DB, Id, Found);
   end Load_Question;

   --  ------------------------------
   --  Create or save the answer.
   --  ------------------------------
   procedure Save_Answer (Model    : in Question_Service;
                          Question : in AWA.Questions.Models.Question_Ref'Class;
                          Answer   : in out AWA.Questions.Models.Answer_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;
      if Answer.Is_Inserted then
         Log.Info ("Updating question {0}", ADO.Identifier'Image (Answer.Get_Id));

      else
         Log.Info ("Creating new answer for {0}", ADO.Identifier'Image (Question.Get_Id));

         --  Check that the user has the create permission on the given workspace.
         AWA.Permissions.Check (Permission => ACL_Answer_Questions.Permission,
                                Entity     => Question);
         Answer.Set_Author (User);
      end if;

      if not Answer.Is_Inserted then
         Answer.Set_Create_Date (Ada.Calendar.Clock);
         Answer.Set_Question (Question);
      else
         Answer.Set_Edit_Date (ADO.Nullable_Time '(Value   => Ada.Calendar.Clock,
                                                   Is_Null => False));
      end if;
      Answer.Save (DB);
      Ctx.Commit;
   end Save_Answer;

   --  ------------------------------
   --  Delete the answer.
   --  ------------------------------
   procedure Delete_Answer (Model  : in Question_Service;
                            Answer : in out AWA.Questions.Models.Answer_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;

      --  Check that the user has the delete permission on the given answer.
      AWA.Permissions.Check (Permission => ACL_Delete_Answer.Permission,
                             Entity     => Answer);


      Answer.Delete (DB);
      Ctx.Commit;
   end Delete_Answer;

   --  ------------------------------
   --  Load the answer.
   --  ------------------------------
   procedure Load_Answer (Model    : in Question_Service;
                          Answer   : in out AWA.Questions.Models.Answer_Ref'Class;
                          Question : in out AWA.Questions.Models.Question_Ref'Class;
                          Id       : in ADO.Identifier) is
      pragma Unreferenced (Model);

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Found : Boolean;
   begin
      Answer.Load (DB, Id, Found);
      Question := Answer.Get_Question;
   end Load_Answer;

end AWA.Questions.Services;
