-----------------------------------------------------------------------
--  awa-questions-modules-tests -- Unit tests for storage service
--  Copyright (C) 2013, 2014, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

with Util.Test_Caller;
with Util.Beans.Basic;
with Util.Beans.Objects;

with Security.Contexts;

with ASF.Contexts.Faces;
with ASF.Contexts.Faces.Mockup;

with AWA.Permissions;
with AWA.Services.Contexts;
with AWA.Questions.Beans;
with AWA.Votes.Beans;
with AWA.Tests.Helpers.Users;
package body AWA.Questions.Modules.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Questions.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Questions.Services.Save_Question",
                       Test_Create_Question'Access);
      Caller.Add_Test (Suite, "Test AWA.Questions.Services.Delete_Question",
                       Test_Delete_Question'Access);
      Caller.Add_Test (Suite, "Test AWA.Questions.Queries question-list",
                       Test_List_Questions'Access);
      Caller.Add_Test (Suite, "Test AWA.Questions.Beans questionVote bean",
                       Test_Question_Vote'Access);
      Caller.Add_Test (Suite, "Test AWA.Questions.Beans questionVote bean (anonymous user)",
                       Test_Question_Vote'Access);
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

      T.Manager := AWA.Questions.Modules.Get_Question_Module;
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
   --  Test deletion of a question.
   --  ------------------------------
   procedure Test_Delete_Question (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Q         : AWA.Questions.Models.Question_Ref;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-storage@test.com");

      T.Manager := AWA.Questions.Modules.Get_Question_Module;
      T.Assert (T.Manager /= null, "There is no question manager");

      Q.Set_Title ("How can I search strings in Ada?");
      Q.Set_Description ("I have two strings that I want to search.  % does not work.");
      T.Manager.Save_Question (Q);
      T.Assert (Q.Is_Inserted, "The new question was not inserted");

      T.Manager.Delete_Question (Q);
   end Test_Delete_Question;

   --  ------------------------------
   --  Test list of questions.
   --  ------------------------------
   procedure Test_List_Questions (T : in out Test) is
      use type Util.Beans.Basic.Readonly_Bean_Access;

      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
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
      T.Assert (List.all in Questions.Beans.Question_List_Bean'Class,
                "The Create_Question_List_Bean returns an invalid bean");
      Count := Questions.Beans.Question_List_Bean'Class (List.all).Questions.Get_Count;

      T.Assert (Count > 0, "The list of question is empty");
   end Test_List_Questions;

   --  ------------------------------
   --  Do a vote on a question through the question vote bean.
   --  ------------------------------
   procedure Do_Vote (T : in out Test) is
      use type Util.Beans.Basic.Readonly_Bean_Access;

      Context : ASF.Contexts.Faces.Mockup.Mockup_Faces_Context;
      Outcome : Ada.Strings.Unbounded.Unbounded_String;
      Bean    : Util.Beans.Basic.Readonly_Bean_Access;
      Vote    : AWA.Votes.Beans.Vote_Bean_Access;
   begin
      Context.Set_Parameter ("id", "1");
      Bean := Context.Get_Bean ("questionVote");
      T.Assert (Bean /= null, "The questionVote bean was not created");

      T.Assert (Bean.all in AWA.Votes.Beans.Vote_Bean'Class,
                "The questionVote is not a Vote_Bean");

      Vote := AWA.Votes.Beans.Vote_Bean (Bean.all)'Access;
      Vote.Rating    := 1;
      Vote.Entity_Id := 1;
      Vote.Vote_Up (Outcome);
   end Do_Vote;

   --  ------------------------------
   --  Test anonymous user voting for a question.
   --  ------------------------------
   procedure Test_Question_Vote_Anonymous (T : in out Test) is
      Sec_Ctx : Security.Contexts.Security_Context;
      Context : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Anonymous (Context, Sec_Ctx);

      Do_Vote (T);
      T.Fail ("Anonymous users should not be allowed to vote");

   exception
      when AWA.Permissions.NO_PERMISSION =>
         null;
   end Test_Question_Vote_Anonymous;

   --  ------------------------------
   --  Test voting for a question.
   --  ------------------------------
   procedure Test_Question_Vote (T : in out Test) is
      Sec_Ctx : Security.Contexts.Security_Context;
      Context : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-voter@test.com");

      Do_Vote (T);
   end Test_Question_Vote;

end AWA.Questions.Modules.Tests;
