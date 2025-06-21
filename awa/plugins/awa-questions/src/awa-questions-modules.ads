-----------------------------------------------------------------------
--  awa-questions-modules -- Module questions
--  Copyright (C) 2012, 2013, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO;

with Security.Permissions;

with ASF.Applications;
with AWA.Modules;
with AWA.Questions.Models;
package AWA.Questions.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "questions";

   --  Define the permissions.
   package ACL_Create_Questions is new Security.Permissions.Definition ("question-create");
   package ACL_Delete_Questions is new Security.Permissions.Definition ("question-delete");
   package ACL_Update_Questions is new Security.Permissions.Definition ("question-update");
   package ACL_Answer_Questions is new Security.Permissions.Definition ("answer-create");
   package ACL_Delete_Answer is new Security.Permissions.Definition ("answer-delete");

   --  The maximum length for a short description.
   SHORT_DESCRIPTION_LENGTH : constant Positive := 200;

   --  ------------------------------
   --  Module questions
   --  ------------------------------
   type Question_Module is new AWA.Modules.Module with private;
   type Question_Module_Access is access all Question_Module'Class;

   --  Initialize the questions module.
   overriding
   procedure Initialize (Plugin : in out Question_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the questions module.
   function Get_Question_Module return Question_Module_Access;

   --  Create or save the question.
   procedure Save_Question (Model    : in Question_Module;
                            Question : in out AWA.Questions.Models.Question_Ref'Class);

   --  Delete the question.
   procedure Delete_Question (Model    : in Question_Module;
                              Question : in out AWA.Questions.Models.Question_Ref'Class);

   --  Load the question.
   procedure Load_Question (Model    : in Question_Module;
                            Question : in out AWA.Questions.Models.Question_Ref'Class;
                            Id       : in ADO.Identifier;
                            Found    : out Boolean);

   --  Create or save the answer.
   procedure Save_Answer (Model    : in Question_Module;
                          Question : in AWA.Questions.Models.Question_Ref'Class;
                          Answer   : in out AWA.Questions.Models.Answer_Ref'Class);

   --  Delete the answer.
   procedure Delete_Answer (Model  : in Question_Module;
                            Answer : in out AWA.Questions.Models.Answer_Ref'Class);

   --  Load the answer.
   procedure Load_Answer (Model    : in Question_Module;
                          Answer   : in out AWA.Questions.Models.Answer_Ref'Class;
                          Question : in out AWA.Questions.Models.Question_Ref'Class;
                          Id       : in ADO.Identifier;
                          Found    : out Boolean);

private

   type Question_Module is new AWA.Modules.Module with null record;

end AWA.Questions.Modules;
