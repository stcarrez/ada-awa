-----------------------------------------------------------------------
--  awa-wikis-tests -- Unit tests for wikis module
--  Copyright (C) 2018, 2019, 2020 Stephane Carrez
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

with ADO;
with Servlet.Streams;
with ASF.Tests;
with AWA.Tests.Helpers.Users;
with AWA.Storages.Beans;
with AWA.Storages.Models;
with AWA.Storages.Services;
with AWA.Storages.Modules;
with AWA.Services.Contexts;
with Security.Contexts;

package body AWA.Wikis.Tests is

   use Ada.Strings.Unbounded;
   use AWA.Tests;
   use type AWA.Storages.Services.Storage_Service_Access;

   package Caller is new Util.Test_Caller (Test, "Wikis.Beans");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Wikis.Beans.Load_List (Anonymous)",
                       Test_Anonymous_Access'Access);
      Caller.Add_Test (Suite, "Test AWA.Wikis.Beans.Save",
                       Test_Create_Wiki'Access);
      Caller.Add_Test (Suite, "Test AWA.Wikis.Beans.Load (missing)",
                       Test_Missing_Page'Access);
      Caller.Add_Test (Suite, "Test AWA.Wikis.Beans.Load (image)",
                       Test_Page_With_Image'Access);
   end Add_Tests;

   --  ------------------------------
   --  Setup an image for the wiki page.
   --  ------------------------------
   procedure Make_Wiki_Image (T : in out Test) is
      Sec_Ctx : Security.Contexts.Security_Context;
      Context : AWA.Services.Contexts.Service_Context;
      Folder  : AWA.Storages.Beans.Folder_Bean;
      Store   : AWA.Storages.Models.Storage_Ref;
      Mgr     : AWA.Storages.Services.Storage_Service_Access;
      Outcome : Ada.Strings.Unbounded.Unbounded_String;
      Path    : constant String
        := Util.Tests.Get_Path ("regtests/files/images/Ada-Lovelace.jpg");
      Wiki      : constant String := To_String (T.Wiki_Ident);
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

      Mgr := AWA.Storages.Modules.Get_Storage_Manager;
      T.Assert (Mgr /= null, "Null storage manager");

      --  Make a storage folder.
      Folder.Module := AWA.Storages.Modules.Get_Storage_Module;
      Folder.Set_Name ("Images");
      Folder.Save (Outcome);

      Store.Set_Folder (Folder);
      Store.Set_Is_Public (True);
      Store.Set_Mime_Type ("image/jpg");
      Store.Set_Name ("Ada-Lovelace.jpg");
      Mgr.Save (Store, Path, AWA.Storages.Models.FILE);

      declare
         Request : Servlet.Requests.Mockup.Request;
         Reply   : Servlet.Responses.Mockup.Response;
         Id      : constant String := ADO.Identifier'Image (Store.Get_Id);
      begin
         T.Image_Ident := To_Unbounded_String (Id (Id'First + 1 .. Id'Last));
         AWA.Tests.Helpers.Users.Login ("test-storage@test.com", Request);
         ASF.Tests.Do_Get (Request, Reply,
                           "/wikis/images/" & Wiki & "/"
                           & Id (Id'First + 1 .. Id'Last)
                           & "/original/Ada-Lovelace.jpg",
                           "wiki-image-get-Ada-Lovelace.jpg");
         ASF.Tests.Assert_Header (T, "Content-Type", "image/jpg", Reply);
         Util.Tests.Assert_Equals (T, Servlet.Responses.SC_OK,
                                   Reply.Get_Status,
                                   "Invalid response for image");

         T.Image_Link := To_Unbounded_String ("/wikis/images/" & Wiki
                                              & "/" & Id (Id'First + 1 .. Id'Last)
                                              & "/default/Ada-Lovelace.jpg");
      end;
   end Make_Wiki_Image;

   --  ------------------------------
   --  Get some access on the wiki as anonymous users.
   --  ------------------------------
   procedure Verify_Anonymous (T     : in out Test;
                               Page  : in String;
                               Title : in String) is
      pragma Unreferenced (Title);
      function Get_Link (Title : in String) return String;

      Wiki      : constant String := To_String (T.Wiki_Ident);
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;

      function Get_Link (Title : in String) return String is
         Stream  : Servlet.Streams.Print_Stream := Reply.Get_Output_Stream;
         Content : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Reply.Read_Content (Content);
         Stream.Write (Content);
         return AWA.Tests.Helpers.Extract_Link (To_String (Content), Title);
      end Get_Link;

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
         declare
            Info    : constant String := Get_Link ("Info");
            History : constant String := Get_Link ("History");
         begin
            Util.Tests.Assert_Matches (T, "/asfunit/wikis/info/[0-9]+/[0-9]+$", Info,
                                    "Invalid wiki info link in the response");
            Util.Tests.Assert_Matches (T, "/asfunit/wikis/history/[0-9]+/[0-9]+$", History,
                                       "Invalid wiki history link in the response");

            --  Get the information page.
            ASF.Tests.Do_Get (Request, Reply, Info (Info'First + 8 .. Info'Last),
                              "wiki-info-" & Page & ".html");
            ASF.Tests.Assert_Contains (T, "wiki-word-list", Reply,
                                       "Wiki info page " & Page & " is invalid");

            --  Get the history page.
            ASF.Tests.Do_Get (Request, Reply, History (History'First + 8 .. History'Last),
                              "wiki-history-" & Page & ".html");
            ASF.Tests.Assert_Contains (T, "wiki-page-version", Reply,
                                       "Wiki history page " & Page & " is invalid");

         end;
      end if;
   end Verify_Anonymous;

   --  ------------------------------
   --  Verify that the wiki lists contain the given page.
   --  ------------------------------
   procedure Verify_List_Contains (T    : in out Test;
                                   Page : in String) is
      Wiki      : constant String := To_String (T.Wiki_Ident);
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Wiki & "/recent",
                        "wiki-list-recent.html");
      ASF.Tests.Assert_Contains (T, "List of pages", Reply,
                                 "Wiki list recent page is invalid");
      ASF.Tests.Assert_Contains (T, "/wikis/view/" & To_String (T.Wiki_Ident)
                                 & "/" & Page, Reply,
                                 "Wiki list recent page does not reference the page");

      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Wiki & "/popular",
                        "wiki-list-popular.html");
      ASF.Tests.Assert_Contains (T, "List of pages", Reply,
                                 "Wiki list popular page is invalid");
      ASF.Tests.Assert_Contains (T, "/wikis/view/" & To_String (T.Wiki_Ident)
                                 & "/" & Page, Reply,
                                 "Wiki list popular page does not reference the page");

      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Wiki & "/name",
                        "wiki-list-name.html");
      ASF.Tests.Assert_Contains (T, "List of pages", Reply,
                                 "Wiki list name page is invalid");
      ASF.Tests.Assert_Contains (T, "/wikis/view/" & To_String (T.Wiki_Ident)
                                 & "/" & Page, Reply,
                                 "Wiki list name page does not reference the page");

      ASF.Tests.Do_Get (Request, Reply, "/wikis/list/" & Wiki & "/name/grid",
                        "wiki-list-name-grid.html");
      ASF.Tests.Assert_Contains (T, "List of pages", Reply,
                                 "Wiki list name/grid page is invalid");
      ASF.Tests.Assert_Contains (T, "/wikis/view/" & To_String (T.Wiki_Ident)
                                 & "/" & Page, Reply,
                                 "Wiki list name/grid page does not reference the page");

   end Verify_List_Contains;

   --  ------------------------------
   --  Test access to the blog as anonymous user.
   --  ------------------------------
   procedure Test_Anonymous_Access (T : in out Test) is
   begin
      T.Verify_Anonymous ("", "");
   end Test_Anonymous_Access;

   --  ------------------------------
   --  Create a wiki page.
   --  ------------------------------
   procedure Create_Page (T       : in out Test;
                          Request : in out Servlet.Requests.Mockup.Request;
                          Reply   : in out Servlet.Responses.Mockup.Response;
                          Name    : in String;
                          Title   : in String) is
   begin
      Request.Set_Parameter ("page-wiki-id", To_String (T.Wiki_Ident));
      Request.Set_Parameter ("post", "1");
      Request.Set_Parameter ("page-title", Title);
      Request.Set_Parameter ("text", "# Main title" & ASCII.LF
                             & "* The wiki page content." & ASCII.LF
                             & "* Second item." & ASCII.LF
                             & ASCII.LF
                             & "![Ada Lovelace](Images/Ada-Lovelace.jpg)");
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

   --  ------------------------------
   --  Test creation of blog by simulating web requests.
   --  ------------------------------
   procedure Test_Create_Wiki (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      Request.Set_Parameter ("title", "The Wiki Space Title");
      Request.Set_Parameter ("post", "1");
      Request.Set_Parameter ("create", "1");
      ASF.Tests.Do_Post (Request, Reply, "/wikis/setup.html", "setup-wiki.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_MOVED_TEMPORARILY,
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
      T.Create_Page (Request, Reply, "WikiPageTestName", "Wiki page title1");
      T.Verify_List_Contains (To_String (T.Page_Ident));

      T.Create_Page (Request, Reply, "WikiSecondPageName", "Wiki page title2");
      T.Verify_List_Contains (To_String (T.Page_Ident));

      T.Create_Page (Request, Reply, "WikiThirdPageName", "Wiki page title3");

      T.Verify_Anonymous ("WikiPageTestName", "Wiki page title1");
      T.Verify_Anonymous ("WikiSecondPageName", "Wiki page title2");
      T.Verify_Anonymous ("WikiThirdPageName", "Wiki page title3");

   end Test_Create_Wiki;

   --  ------------------------------
   --  Test getting a wiki page which does not exist.
   --  ------------------------------
   procedure Test_Missing_Page (T : in out Test) is
      Wiki      : constant String := To_String (T.Wiki_Ident);
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/wikis/view/" & Wiki & "/MissingPage",
                        "wiki-page-missing.html");
      ASF.Tests.Assert_Matches (T, ".title.Wiki page does not exist./title.", Reply,
                                "Wiki page 'MissingPage' is invalid",
                                Servlet.Responses.SC_NOT_FOUND);
      ASF.Tests.Assert_Matches (T, ".h2.MissingPage./h2.", Reply,
                                "Wiki page 'MissingPage' header is invalid",
                                Servlet.Responses.SC_NOT_FOUND);
   end Test_Missing_Page;

   --  ------------------------------
   --  Test creation of wiki page with an image.
   --  ------------------------------
   procedure Test_Page_With_Image (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
      Wiki      : constant String := To_String (T.Wiki_Ident);
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);

      T.Make_Wiki_Image;
      T.Create_Page (Request, Reply, "WikiImageTest", "Wiki image title3");

      ASF.Tests.Do_Get (Request, Reply, "/wikis/view/" & Wiki & "/WikiImageTest",
                        "wiki-image-test.html");
      ASF.Tests.Assert_Matches (T, "<img src=./wikis/images/[0-9]*/[0-9]*"
                                & "/default/Ada-Lovelace.jpg.* alt=.Ada Lovelace.></img>",
                                Reply,
                                "Wiki page missing image link",
                                Servlet.Responses.SC_OK);

      ASF.Tests.Do_Get (Request, Reply,
                        To_String (T.Image_Link),
                        "wiki-image-get-Ada-Lovelace.jpg");
      ASF.Tests.Assert_Header (T, "Content-Type", "image/jpg", Reply);
      Util.Tests.Assert_Equals (T, Servlet.Responses.SC_OK,
                                Reply.Get_Status,
                                "Invalid response for image");

      ASF.Tests.Do_Get (Request, Reply, "/wikis/image-info/" & Wiki & "/"
                        & To_String (T.Image_Ident) & "/Images/Ada-Lovelace.jpg",
                        "wiki-image-info.html");
      ASF.Tests.Assert_Contains (T, "<title>Image information</title>", Reply,
                                 "Wiki image information page is invalid");
      ASF.Tests.Assert_Matches (T, "<dt>File name</dt>",
                                Reply,
                                "Wiki image information invalid name (1)",
                                Servlet.Responses.SC_OK);
      ASF.Tests.Assert_Matches (T, "<dd>Images/Ada-Lovelace.jpg</dd>",
                                Reply,
                                "Wiki image information invalid name (2)",
                                Servlet.Responses.SC_OK);

   end Test_Page_With_Image;

end AWA.Wikis.Tests;
