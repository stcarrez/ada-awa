-----------------------------------------------------------------------
--  awa-storages-tests -- Unit tests for storages module
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

with Ada.Strings.Unbounded;

with Util.Test_Caller;

with ASF.Tests;
with AWA.Tests.Helpers.Users;
with Servlet.requests.Mockup;
with Servlet.Responses.Mockup;

package body AWA.Storages.Tests is

   package Caller is new Util.Test_Caller (Test, "Storages.Beans");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Storages.Beans.Create",
                       Test_Create_Document'Access);
      Caller.Add_Test (Suite, "Test AWA.Storages.Servlets (missing)",
                       Test_Missing_Document'Access);
   end Add_Tests;

   --  ------------------------------
   --  Get some access on the wiki as anonymous users.
   --  ------------------------------
   procedure Verify_Anonymous (T     : in out Test;
                               Page  : in String;
                               Title : in String) is
      pragma Unreferenced (Title);

      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/storages/documents.html",
                        "storage-anonymous-list.html");
      ASF.Tests.Assert_Contains (T, "List of pages", Reply,
                                 "Wiki list recent page is invalid");
   end Verify_Anonymous;

   --  ------------------------------
   --  Verify that the wiki lists contain the given page.
   --  ------------------------------
   procedure Verify_List_Contains (T    : in out Test;
                                   Page : in String) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/storages/documents.html",
                        "storage-list.html");
      ASF.Tests.Assert_Contains (T, "Documents of the workspace", Reply,
                                 "List of documents is invalid");
   end Verify_List_Contains;

   --  ------------------------------
   --  Test access to the blog as anonymous user.
   --  ------------------------------
   procedure Test_Anonymous_Access (T : in out Test) is
   begin
      T.Verify_Anonymous ("", "");
   end Test_Anonymous_Access;

   --  ------------------------------
   --  Test creation of document by simulating web requests.
   --  ------------------------------
   procedure Test_Create_Document (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      AWA.Tests.Helpers.Users.Login ("test-storage@test.com", Request);
      Request.Set_Parameter ("folder-name", "Test Folder Name");
      Request.Set_Parameter ("storage-folder-create-form", "1");
      Request.Set_Parameter ("storage-folder-create-button", "1");
      ASF.Tests.Do_Post (Request, Reply,
                         "/storages/forms/folder-create.html",
                         "folder-create-form.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_OK,
                "Invalid response after folder creation");

      ASF.Tests.Do_Get (Request, Reply, "/storages/documents.html",
                        "storage-list.html");
      ASF.Tests.Assert_Contains (T, "Documents of the workspace", Reply,
                                 "List of documents is invalid (title)");
      ASF.Tests.Assert_Contains (T, "Test Folder Name", Reply,
                                 "List of documents is invalid (content)");
   end Test_Create_Document;

   --  ------------------------------
   --  Test getting a document which does not exist.
   --  ------------------------------
   procedure Test_Missing_Document (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/storages/files/12345345/view/missing.pdf",
                        "storage-file-missing.html");
      ASF.Tests.Assert_Matches (T, ".title.Page not found./title.", Reply,
                                "Page for a missing document is invalid",
                                Servlet.Responses.SC_NOT_FOUND);
   end Test_Missing_Document;

end AWA.Storages.Tests;
