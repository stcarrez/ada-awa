-----------------------------------------------------------------------
--  awa-questions-tests -- Unit tests for questions module
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;
with Ada.Strings.Unbounded;

package AWA.Questions.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Question_Ident : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Get some access on the wiki page as anonymous users.
   procedure Verify_Anonymous (T     : in out Test;
                               Page  : in String;
                               Title : in String);

   --  Verify that the wiki lists contain the given page.
   procedure Verify_List_Contains (T     : in out Test;
                                   Id    : in String;
                                   Title : in String);

   --  Test access to the question as anonymous user.
   procedure Test_Anonymous_Access (T : in out Test);

   --  Test creation of question by simulating web requests.
   procedure Test_Create_Question (T : in out Test);

   --  Test answer of question by simulating web requests.
   procedure Test_Answer_Question (T : in out Test);

   --  Test getting a wiki page which does not exist.
   procedure Test_Missing_Page (T : in out Test);

end AWA.Questions.Tests;
