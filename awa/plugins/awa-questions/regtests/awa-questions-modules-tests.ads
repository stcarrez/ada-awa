-----------------------------------------------------------------------
--  awa-questions-modules-tests -- Unit tests for question service
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
