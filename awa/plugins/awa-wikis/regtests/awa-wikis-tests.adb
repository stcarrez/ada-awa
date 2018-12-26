-----------------------------------------------------------------------
--  awa-wikis-tests -- Unit tests for wikis module
--  Copyright (C) 2018 Stephane Carrez
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
with Util.Strings;

with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Tests;
with AWA.Tests.Helpers.Users;

package body AWA.Wikis.Tests is

   use Ada.Strings.Unbounded;
   use AWA.Tests;

   package Caller is new Util.Test_Caller (Test, "Wikis");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Wikis.Beans.Load_List (Anonymous)",
                       Test_Anonymous_Access'Access);
      Caller.Add_Test (Suite, "Test AWA.Wikis.Beans.Save",
                       Test_Create_Wiki'Access);
   end Add_Tests;

   --  ------------------------------
   --  Get some access on the wiki as anonymous users.
   --  ------------------------------
   procedure Verify_Anonymous (T     : in out Test;
                               Page  : in String;
                               Title : in String) is
      Wiki      : constant String := To_String (T.Wiki_Ident);
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Wiki & "/recent",
                        "wiki-list-recent.html");
      ASF.Tests.Assert_Contains (T, "List of pages", Reply,
                                 "Wiki list recent page is invalid");

      ASF.Tests.Do_Get (Request, Reply, "/wikis/tags/" & Wiki,
                        "wiki-list-tagged.html");
      ASF.Tests.Assert_Contains (T, "List of pages", Reply,
                                 "Wiki tag page is invalid");

      if Page'Length > 0 then
         ASF.Tests.Do_Get (Request, Reply, "/wikis/view/" & Wiki & "/" & Page,
                           "wiki-page-" & Page & ".html");
         ASF.Tests.Assert_Contains (T, "The wiki page content", Reply,
                                    "Wiki page " & Page & " is invalid");
      end if;
   end Verify_Anonymous;

   --  ------------------------------
   --  Verify that the wiki lists contain the given page.
   --  ------------------------------
   procedure Verify_List_Contains (T    : in out Test;
                                   Page : in String) is
      Wiki      : constant String := To_String (T.Wiki_Ident);
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Wiki & "/recent",
                        "wiki-list-recent.html");
      ASF.Tests.Assert_Contains (T, "List of pages", Reply,
                                 "Wiki list recent page is invalid");
      ASF.Tests.Assert_Contains (T, "/wikis/view/" & To_String (T.Wiki_Ident)
                                 & "/" & Page, Reply,
                                 "Wiki list recent page does not reference the page");

   end Verify_List_Contains;

   --  ------------------------------
   --  Test access to the blog as anonymous user.
   --  ------------------------------
   procedure Test_Anonymous_Access (T : in out Test) is
   begin
      T.Verify_Anonymous ("", "");
   end Test_Anonymous_Access;

   --  ------------------------------
   --  Test creation of blog by simulating web requests.
   --  ------------------------------
   procedure Test_Create_Wiki (T : in out Test) is
      procedure Create_Page (Name : in String; Title : in String);

      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
      Uuid      : constant String := Util.Tests.Get_Uuid;

      procedure Create_Page (Name : in String; Title : in String) is
      begin
         Request.Set_Parameter ("page-wiki-id", To_String (T.Wiki_Ident));
         Request.Set_Parameter ("post", "1");
         Request.Set_Parameter ("page-title", Title);
         Request.Set_Parameter ("text", "# Main title" & ASCII.LF
                                & "* The wiki page content." & ASCII.LF
                                & "* Second item." & ASCII.LF);
         Request.Set_Parameter ("name", Name);
         Request.Set_Parameter ("comment", "Created wiki page " & Name);
         Request.Set_Parameter ("save", "1");
         Request.Set_Parameter ("page-is-public", "1");
         Request.Set_Parameter ("wiki-format", "FORMAT_MARKDOWN");
         ASF.Tests.Do_Post (Request, Reply, "/wikis/create.html", "create-wiki.html");

         T.Page_Ident := Helpers.Extract_Redirect (Reply, "/asfunit/wikis/view/"
                                                   & To_String (T.Wiki_Ident) & "/");

         Util.Tests.Assert_Equals (T, Name, To_String (T.Page_Ident),
                                   "Invalid redirect after wiki page creation");

         --  Remove the 'wikiPage' bean from the request so that we get a new instance
         --  for the next call.
         Request.Remove_Attribute ("wikiPage");
      end Create_Page;

   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      Request.Set_Parameter ("title", "The Wiki Space Title");
      Request.Set_Parameter ("post", "1");
      Request.Set_Parameter ("create", "1");
      ASF.Tests.Do_Post (Request, Reply, "/wikis/setup.html", "setup-wiki.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY,
                "Invalid response after wiki space creation");
      declare
         Ident : constant String
           := Helpers.Extract_Redirect (Reply, "/asfunit/wikis/list/");
         Pos   : constant Natural
           := Util.Strings.Index (Ident, '/');
      begin
         Util.Tests.Assert_Matches (T, "^[0-9]+/recent/grid$", Ident,
                                    "Invalid wiki space identifier in the response");
         T.Wiki_Ident := To_Unbounded_String (Ident (Ident'First .. Pos - 1));
      end;
      Create_Page ("WikiPageTestName", "Wiki page title1");
      T.Verify_List_Contains (To_String (T.Page_Ident));

      Create_Page ("WikiSecondPageName", "Wiki page title2");
      T.Verify_List_Contains (To_String (T.Page_Ident));

      Create_Page ("WikiThirdPageName", "Wiki page title3");

      T.Verify_Anonymous ("WikiPageTestName", "Wiki page title1");
      T.Verify_Anonymous ("WikiSecondPageName", "Wiki page title2");
      T.Verify_Anonymous ("WikiThirdPageName", "Wiki page title3");

   end Test_Create_Wiki;

end AWA.Wikis.Tests;
