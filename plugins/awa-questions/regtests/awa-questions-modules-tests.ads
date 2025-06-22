-----------------------------------------------------------------------
--  awa-questions-modules-tests -- Unit tests for question service
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Questions.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Manager : AWA.Questions.Modules.Question_Module_Access;
   end record;

   --  Test creation of a question.
   procedure Test_Create_Question (T : in out Test);

   --  Test deletion of a question.
   procedure Test_Delete_Question (T : in out Test);

   --  Test list of questions.
   procedure Test_List_Questions (T : in out Test);

   --  Test anonymous user voting for a question.
   procedure Test_Question_Vote_Anonymous (T : in out Test);

   --  Test voting for a question.
   procedure Test_Question_Vote (T : in out Test);

private

   --  Do a vote on a question through the question vote bean.
   procedure Do_Vote (T : in out Test);

end AWA.Questions.Modules.Tests;
