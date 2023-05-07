-----------------------------------------------------------------------
--  awa-questions-tests -- Unit tests for questions module
--  Copyright (C) 2018, 2019, 2023 Stephane Carrez
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

with Util.Test_Caller;

with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Tests;
with AWA.Tests.Helpers.Users;

package body AWA.Questions.Tests is

   use Ada.Strings.Unbounded;
   use AWA.Tests;

   package Caller is new Util.Test_Caller (Test, "Questions.Beans");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Questions.Beans.Load_List (Anonymous)",
                       Test_Anonymous_Access'Access);
      Caller.Add_Test (Suite, "Test AWA.Questions.Beans.Save",
                       Test_Create_Question'Access);
      Caller.Add_Test (Suite, "Test AWA.Questions.Beans.Load (missing)",
                       Test_Missing_Page'Access);
      Caller.Add_Test (Suite, "Test AWA.Questions.Beans.Save (answer)",
                       Test_Answer_Question'Access);
   end Add_Tests;

   --  ------------------------------
   --  Get some access on the wiki as anonymous users.
   --  ------------------------------
   procedure Verify_Anonymous (T     : in out Test;
                               Page  : in String;
                               Title : in String) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/questions/list.html",
                        "question-list.html");
      ASF.Tests.Assert_Contains (T, "<title>Questions</title>", Reply,
                                 "Questions list page is invalid");

      ASF.Tests.Do_Get (Request, Reply, "/questions/tags/tag",
                        "question-list-tagged.html");
      ASF.Tests.Assert_Contains (T, "<title>Questions</title>", Reply,
                                 "Questions tag page is invalid");

      if Page'Length > 0 then
         ASF.Tests.Do_Get (Request, Reply, "/questions/view/" & Page,
                           "question-page-" & Page & ".html");
         ASF.Tests.Assert_Contains (T, Title, Reply,
                                    "Question page " & Page & " is invalid");
         ASF.Tests.Assert_Matches (T, ".input type=.hidden. name=.question-id. "
                                   & "value=.[0-9]+. id=.question-id.../input", Reply,
                                    "Question page " & Page & " is invalid");
      end if;
   end Verify_Anonymous;

   --  ------------------------------
   --  Verify that the question list contains the given question.
   --  ------------------------------
   procedure Verify_List_Contains (T     : in out Test;
                                   Id    : in String;
                                   Title : in String) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/questions/list.html",
                        "question-list-recent.html");
      ASF.Tests.Assert_Contains (T, "Questions", Reply,
                                 "List of questions page is invalid");
      ASF.Tests.Assert_Contains (T, "/questions/view/" & Id, Reply,
                                 "List of questions page does not reference the page");
      ASF.Tests.Assert_Contains (T, Title, Reply,
                                 "List of questions page does not contain the question");

   end Verify_List_Contains;

   --  ------------------------------
   --  Test access to the question as anonymous user.
   --  ------------------------------
   procedure Test_Anonymous_Access (T : in out Test) is
   begin
      T.Verify_Anonymous ("", "");
   end Test_Anonymous_Access;

   --  ------------------------------
   --  Test creation of question by simulating web requests.
   --  ------------------------------
   procedure Test_Create_Question (T : in out Test) is
      procedure Create_Question (Title : in String);

      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;

      procedure Create_Question (Title : in String) is
      begin
         ASF.Tests.Do_Get (Request, Reply, "/questions/ask.html", "questions-ask-get.html");

         Request.Set_Parameter ("title", Title);
         Request.Set_Parameter ("text", "# Main title" & ASCII.LF
                                & "* The question content." & ASCII.LF
                                & "* Second item." & ASCII.LF);
         Request.Set_Parameter ("save", "1");
         ASF.Tests.Set_CSRF (Request, "post", "questions-ask-get.html");
         ASF.Tests.Do_Post (Request, Reply, "/questions/ask.html", "questions-ask.html");

         T.Question_Ident := Helpers.Extract_Redirect (Reply, "/asfunit/questions/view/");

         Util.Tests.Assert_Matches (T, "[0-9]+$", To_String (T.Question_Ident),
                                   "Invalid redirect after question creation");

         --  Remove the 'question' bean from the request so that we get a new instance
         --  for the next call.
         Request.Remove_Attribute ("question");
      end Create_Question;

   begin
      AWA.Tests.Helpers.Users.Login ("test-question@test.com", Request);

      Create_Question ("Question 1 page title1");
      T.Verify_List_Contains (To_String (T.Question_Ident), "Question 1 page title1");
      T.Verify_Anonymous (To_String (T.Question_Ident), "Question 1 page title1");

      Create_Question ("Question 2 page title2");
      T.Verify_List_Contains (To_String (T.Question_Ident), "Question 2 page title2");
      T.Verify_Anonymous (To_String (T.Question_Ident), "Question 2 page title2");

      Create_Question ("Question 3 page title3");
      T.Verify_List_Contains (To_String (T.Question_Ident), "Question 3 page title3");
      T.Verify_Anonymous (To_String (T.Question_Ident), "Question 3 page title3");

   end Test_Create_Question;

   --  ------------------------------
   --  Test getting a wiki page which does not exist.
   --  ------------------------------
   procedure Test_Missing_Page (T : in out Test) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/questions/view.html?id=12345678",
                        "question-page-missing.html");
      ASF.Tests.Assert_Matches (T, "<title>Question not found</title>", Reply,
                                "Question page title '12345678' is invalid",
                                ASF.Responses.SC_NOT_FOUND);
      ASF.Tests.Assert_Matches (T, "question.*removed", Reply,
                                "Question page content '12345678' is invalid",
                                ASF.Responses.SC_NOT_FOUND);
   end Test_Missing_Page;

   --  ------------------------------
   --  Test answer of question by simulating web requests.
   --  ------------------------------
   procedure Test_Answer_Question (T : in out Test) is
      procedure Create_Answer (Content : in String);

      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;

      procedure Create_Answer (Content : in String) is
      begin
         ASF.Tests.Do_Get (Request, Reply, "/questions/forms/answer-form.html",
                           "questions-answer-get.html");

         Request.Set_Parameter ("question-id", To_String (T.Question_Ident));
         Request.Set_Parameter ("answer-id", "");
         Request.Set_Parameter ("text", Content);
         Request.Set_Parameter ("save", "1");
         ASF.Tests.Set_CSRF (Request, "post", "questions-answer-get.html");
         ASF.Tests.Do_Post (Request, Reply, "/questions/forms/answer-form.html",
                            "questions-answer.html");

         ASF.Tests.Assert_Contains (T, "/questions/view/" &  To_String (T.Question_Ident),
                                    Reply,
                                    "Answer response is invalid");

         --  Remove the 'question' bean from the request so that we get a new instance
         --  for the next call.
         Request.Remove_Attribute ("answer");
      end Create_Answer;

   begin
      AWA.Tests.Helpers.Users.Login ("test-answer@test.com", Request);

      Create_Answer ("Answer content 1");
      T.Verify_Anonymous (To_String (T.Question_Ident), "Answer content 1");

      Create_Answer ("Answer content 2");
      T.Verify_Anonymous (To_String (T.Question_Ident), "Answer content 1");
      T.Verify_Anonymous (To_String (T.Question_Ident), "Answer content 2");

      Create_Answer ("Answer content 3");
      T.Verify_Anonymous (To_String (T.Question_Ident), "Answer content 1");
      T.Verify_Anonymous (To_String (T.Question_Ident), "Answer content 2");
      T.Verify_Anonymous (To_String (T.Question_Ident), "Answer content 3");

   end Test_Answer_Question;

end AWA.Questions.Tests;
