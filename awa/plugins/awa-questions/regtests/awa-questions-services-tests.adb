-----------------------------------------------------------------------
--  awa-questions-services-tests -- Unit tests for storage service
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
with Ada.Streams;
with Ada.Strings.Unbounded;

with Util.Test_Caller;
with Util.Beans.Basic;
with Util.Beans.Objects;

with ADO;
with ADO.Objects;

with Security.Contexts;

with AWA.Services.Contexts;
with AWA.Questions.Modules;
with AWA.Questions.Beans;
with AWA.Tests.Helpers.Users;
package body AWA.Questions.Services.Tests is

   use Util.Tests;
   use ADO;

   package Caller is new Util.Test_Caller (Test, "Questions.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Questions.Services.Save_Question",
                       Test_Create_Question'Access);
      Caller.Add_Test (Suite, "Test AWA.Questions.Queries question-list",
                       Test_List_Questions'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of a question.
   --  ------------------------------
   procedure Test_Create_Question (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Q         : AWA.Questions.Models.Question_Ref;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-storage@test.com");

      T.Manager := AWA.Questions.Modules.Get_Question_Manager;
      T.Assert (T.Manager /= null, "There is no question manager");

      Q.Set_Title ("How can I append strings in Ada?");
      Q.Set_Description ("I have two strings that I want to concatenate.  + does not work.");
      T.Manager.Save_Question (Q);
      T.Assert (Q.Is_Inserted, "The new question was not inserted");

      Q.Set_Description ("I have two strings that I want to concatenate.  + does not work. "
                        & "I also tried '.' without success.  What should I do?");
      T.Manager.Save_Question (Q);

      Q.Set_Description ("I have two strings that I want to concatenate.  + does not work. "
                         & "I also tried '.' without success.  What should I do? "
                         & "This is a stupid question for someone who reads this file. "
                         & "But I need some long text for the unit test.");
      T.Manager.Save_Question (Q);
   end Test_Create_Question;

   --  ------------------------------
   --  Test list of questions.
   --  ------------------------------
   procedure Test_List_Questions (T : in out Test) is
      use AWA.Questions.Models;
      use type Util.Beans.Basic.Readonly_Bean_Access;

      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Q         : AWA.Questions.Models.Question_Ref;
      Module    : AWA.Questions.Modules.Question_Module_Access;
      List      : Util.Beans.Basic.Readonly_Bean_Access;
      Bean      : Util.Beans.Objects.Object;
      Count     : Natural;
   begin
      AWA.Tests.Helpers.Users.Anonymous (Context, Sec_Ctx);
      Module := AWA.Questions.Modules.Get_Question_Module;
      List := AWA.Questions.Beans.Create_Question_List_Bean (Module);
      T.Assert (List /= null, "The Create_Question_List_Bean returned null");

      Bean := Util.Beans.Objects.To_Object (List);
      T.Assert (not Util.Beans.Objects.Is_Null (Bean), "The list bean should not be null");
      Count := Questions.Models.Question_Info_List_Bean'Class (List.all).Get_Count;

      T.Assert (Count > 0, "The list of question is empty");
   end Test_List_Questions;

end AWA.Questions.Services.Tests;
