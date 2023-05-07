-----------------------------------------------------------------------
--  awa-wikis-modules-tests -- Unit tests for wikis service
--  Copyright (C) 2015, 2017, 2018, 2023 Stephane Carrez
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
with ASF.Tests;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with AWA.Tests.Helpers;
with AWA.Tests.Helpers.Users;
with AWA.Services.Contexts;
with Security.Contexts;

package body AWA.Wikis.Modules.Tests is

   use ASF.Tests;
   use Util.Tests;
   use type ADO.Identifier;

   package Caller is new Util.Test_Caller (Test, "Wikis.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Wikis.Modules.Create_Wiki_Space",
                       Test_Create_Wiki_Space'Access);
      Caller.Add_Test (Suite, "Test AWA.Wikis.Modules.Create_Wiki_Page",
                       Test_Create_Wiki_Page'Access);
      Caller.Add_Test (Suite, "Test AWA.Wikis.Modules.Create_Wiki_Content",
                       Test_Create_Wiki_Content'Access);
      Caller.Add_Test (Suite, "Test AWA.Wikis.Modules.Wiki_Page",
                       Test_Wiki_Page'Access);
      Caller.Add_Test (Suite, "Test AWA.Wikis.Beans.Save",
                       Test_Update_Page'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of a wiki space.
   --  ------------------------------
   procedure Test_Create_Wiki_Space (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      W         : AWA.Wikis.Models.Wiki_Space_Ref;
      W2        : AWA.Wikis.Models.Wiki_Space_Ref;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

      T.Manager := AWA.Wikis.Modules.Get_Wiki_Module;
      T.Assert (T.Manager /= null, "There is no wiki manager");

      W.Set_Name ("Test wiki space");
      T.Manager.Create_Wiki_Space (W);
      T.Assert (W.Is_Inserted, "The new wiki space was not created");

      W.Set_Name ("Test wiki space update");
      W.Set_Is_Public (True);
      T.Manager.Save_Wiki_Space (W);

      T.Manager.Load_Wiki_Space (Wiki => W2,
                                 Id   => W.Get_Id);
      Util.Tests.Assert_Equals (T, "Test wiki space update", String '(W2.Get_Name),
                                "Invalid wiki space name");
   end Test_Create_Wiki_Space;

   --  ------------------------------
   --  Test creation of a wiki page.
   --  ------------------------------
   procedure Test_Create_Wiki_Page (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      W         : AWA.Wikis.Models.Wiki_Space_Ref;
      P         : AWA.Wikis.Models.Wiki_Page_Ref;
      C         : AWA.Wikis.Models.Wiki_Content_Ref;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

      T.Manager := AWA.Wikis.Modules.Get_Wiki_Module;
      T.Assert (T.Manager /= null, "There is no wiki manager");

      W.Set_Name ("Test wiki space");
      T.Manager.Create_Wiki_Space (W);

      P.Set_Name ("Private");
      P.Set_Title ("The page title");
      C.Set_Content ("Something");
      T.Manager.Create_Wiki_Page (W, P, C);
      T.Assert (P.Is_Inserted, "The new wiki page was not created");

      P.Set_Content (AWA.Wikis.Models.Null_Wiki_Content);
      C := AWA.Wikis.Models.Null_Wiki_Content;
      P := AWA.Wikis.Models.Null_Wiki_Page;
      P.Set_Name ("Public");
      P.Set_Title ("The page title (public)");
      P.Set_Is_Public (True);
      T.Manager.Create_Wiki_Page (W, P, C);
      T.Assert (P.Is_Inserted, "The new wiki page was not created");
      T.Assert (P.Get_Is_Public, "The new wiki page is not public");

      P.Set_Content (AWA.Wikis.Models.Null_Wiki_Content);
   end Test_Create_Wiki_Page;

   --  ------------------------------
   --  Test creation of a wiki page content.
   --  ------------------------------
   procedure Test_Create_Wiki_Content (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      W         : AWA.Wikis.Models.Wiki_Space_Ref;
      P         : AWA.Wikis.Models.Wiki_Page_Ref;
      C         : AWA.Wikis.Models.Wiki_Content_Ref;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

      T.Manager := AWA.Wikis.Modules.Get_Wiki_Module;
      T.Assert (T.Manager /= null, "There is no wiki manager");

      W.Set_Name ("Test wiki space");
      T.Manager.Create_Wiki_Space (W);
      T.Wiki_Id := W.Get_Id;

      P.Set_Name ("PrivatePage");
      P.Set_Title ("The page title");
      P.Set_Is_Public (False);
      T.Manager.Create_Wiki_Page (W, P, C);
      T.Private_Id := P.Get_Id;

      C := AWA.Wikis.Models.Null_Wiki_Content;
      C.Set_Format (AWA.Wikis.Models.FORMAT_MARKDOWN);
      C.Set_Content ("-- Title" & ASCII.LF & "A paragraph");
      C.Set_Save_Comment ("A first version");
      T.Manager.Create_Wiki_Content (P, C);
      T.Assert (C.Is_Inserted, "The new wiki content was not created");
      T.Assert (not C.Get_Author.Is_Null, "The wiki content has an author");
      T.Assert (C.Get_Page_Id /= ADO.NO_IDENTIFIER,
                "The wiki content is associated with the wiki page");

      P := AWA.Wikis.Models.Null_Wiki_Page;
      C := AWA.Wikis.Models.Null_Wiki_Content;
      P.Set_Name ("PublicPage");
      P.Set_Title ("The public page title");
      P.Set_Is_Public (True);
      T.Manager.Create_Wiki_Page (W, P, C);
      T.Public_Id := P.Get_Id;

      C.Set_Format (AWA.Wikis.Models.FORMAT_MARKDOWN);
      C.Set_Content ("-- Title" & ASCII.LF & "A paragraph" & ASCII.LF
                     & "[Link](http://mylink.com)" & ASCII.LF
                     & "[Image](my-image.png)");
      C.Set_Save_Comment ("A first version");
      T.Manager.Create_Wiki_Content (P, C);
      T.Assert (C.Is_Inserted, "The new wiki content was not created");
      T.Assert (not C.Get_Author.Is_Null, "The wiki content has an author");
      T.Assert (C.Get_Page_Id /= ADO.NO_IDENTIFIER,
                "The wiki content is associated with the wiki page");
   end Test_Create_Wiki_Content;

   --  ------------------------------
   --  Test getting the wiki page as well as info, history pages.
   --  ------------------------------
   procedure Test_Wiki_Page (T : in out Test) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
      Ident     : constant String := Util.Strings.Image (Natural (T.Wiki_Id));
      Pub_Ident : constant String := Util.Strings.Image (Natural (T.Public_Id));
   begin
      ASF.Tests.Do_Get (Request, Reply, "/wikis/view/" & Ident & "/PublicPage",
                        "wiki-public-1.html");
      Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status,
                     "Invalid response (PublicPage)");
      Assert_Matches (T, ".*The public page title.*", Reply,
                      "Invalid PublicPage page returned",
                      Status => ASF.Responses.SC_OK);

      ASF.Tests.Do_Get (Request, Reply, "/wikis/info/" & Ident & "/" & Pub_Ident,
                        "wiki-public-info-1.html");
      Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status,
                     "Invalid response (PublicPage info)");
      Assert_Matches (T, ".*The public page title.*", Reply,
                      "Invalid PublicPage info page returned",
                      Status => ASF.Responses.SC_OK);

      ASF.Tests.Do_Get (Request, Reply, "/wikis/history/" & Ident & "/" & Pub_Ident,
                        "wiki-public-history-1.html");
      Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status,
                     "Invalid response (PublicPage history)");
      Assert_Matches (T, ".*The public page title.*", Reply,
                      "Invalid PublicPage info page returned",
                      Status => ASF.Responses.SC_OK);

      Request.Remove_Attribute ("wikiView");
      ASF.Tests.Do_Get (Request, Reply, "/wikis/view/" & Ident & "/PrivatePage",
                        "wiki-private-1.html");
      Assert_Equals (T, ASF.Responses.SC_FORBIDDEN, Reply.Get_Status,
                     "Invalid response (PrivatePage)");
      Assert_Matches (T, ".*Protected Wiki Page.*", Reply, "Invalid PrivatePage page returned",
                      Status => ASF.Responses.SC_FORBIDDEN);

      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Ident & "/recent",
                        "wiki-list-recent-1.html");
      Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status,
                     "Invalid response (list/recent)");

      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Ident & "/recent/grid",
                        "wiki-list-grid-1.html");
      Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status,
                     "Invalid response (list/recent/grid)");

      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Ident & "/popular",
                        "wiki-list-popular-1.html");
      Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status,
                     "Invalid response (list/popular)");

      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Ident & "/popular/grid",
                        "wiki-list-popular-1.html");
      Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status,
                     "Invalid response (list/popular/grid)");
   end Test_Wiki_Page;

   --  ------------------------------
   --  Test updating the wiki page through a POST request.
   --  ------------------------------
   procedure Test_Update_Page (T : in out Test) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
      Ident     : constant String := Util.Strings.Image (Natural (T.Wiki_Id));
      Pub_Ident : constant String := Util.Strings.Image (Natural (T.Public_Id));
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/wikis/edit/" & Ident & "/PublicPage",
                        "update-wiki-get.html");

      Request.Set_Parameter ("title", "The Blog Title");
      Request.Set_Parameter ("page-id", Pub_Ident);
      Request.Set_Parameter ("page-wiki-id", Ident);
      Request.Set_Parameter ("name", "NewPageName");
      Request.Set_Parameter ("page-title", "New Page Title");
      Request.Set_Parameter ("text", "== Title ==" & ASCII.LF & "A paragraph" & ASCII.LF
                             & "[[http://mylink.com|Link]]" & ASCII.LF
                             & "[[Image:my-image.png|My Picture]]" & ASCII.LF
                             & "== Last header ==" & ASCII.LF);
      Request.Set_Parameter ("comment", "Update through test post simulation");
      Request.Set_Parameter ("page-is-public", "TRUE");
      Request.Set_Parameter ("wiki-format", "FORMAT_MEDIAWIKI");
      Request.Set_Parameter ("qtags[1]", "Test-Tag");
      Request.Set_Parameter ("save", "1");
      ASF.Tests.Set_CSRF (Request, "post", "update-wiki-get.html");
      ASF.Tests.Do_Post (Request, Reply, "/wikis/edit/" & Ident & "/PublicPage",
                         "update-wiki.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY,
                "Invalid response after wiki update");
      declare
         Result : constant String
            := AWA.Tests.Helpers.Extract_Redirect (Reply, "/asfunit/wikis/view/");
      begin
         Util.Tests.Assert_Equals (T, Ident & "/NewPageName", Result,
                                   "The page name was not updated");

         ASF.Tests.Do_Get (Request, Reply, "/wikis/view/" & Result, "wiki-public-2.html");
         Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status,
                        "Invalid response (NewPageName)");
         Assert_Matches (T, ".*Last header.*", Reply,
                         "Last header is present in the response",
                         Status => ASF.Responses.SC_OK);

         ASF.Tests.Do_Get (Request, Reply, "/wikis/info/" & Ident & "/"
                           & Pub_Ident, "wiki-info-2.html");
         Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status,
                        "Invalid response (info NewPageName)");
         Assert_Matches (T, ".*wiki-image-name.*my-image.png.*", Reply,
                         "The info page must list the image",
                         Status => ASF.Responses.SC_OK);
      end;
   end Test_Update_Page;

end AWA.Wikis.Modules.Tests;
