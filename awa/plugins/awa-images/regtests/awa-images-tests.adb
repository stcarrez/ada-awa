-----------------------------------------------------------------------
--  awa-images-tests -- Unit tests for images module
--  Copyright (C) 2018, 2019, 2020, 2021, 2022 Stephane Carrez
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
with Ada.Directories;
with Util.Test_Caller;
with Util.Strings;

with ADO;
with Servlet.Requests.Mockup;
with Servlet.Responses.Mockup;
with ASF.Tests;
with AWA.Tests.Helpers.Users;

package body AWA.Images.Tests is

   use Ada.Strings.Unbounded;
   use ADO;

   package Caller is new Util.Test_Caller (Test, "Images.Beans");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Images.Beans.Create",
                       Test_Create_Image'Access);
      Caller.Add_Test (Suite, "Test AWA.Images.Servlets (missing)",
                       Test_Missing_Image'Access);
   end Add_Tests;

   --  ------------------------------
   --  Get some access on the wiki as anonymous users.
   --  ------------------------------
   procedure Verify_Anonymous (T     : in out Test;
                               Page  : in String;
                               Title : in String) is
      pragma Unreferenced (Page, Title);

      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/storages/images.html",
                        "images-anonymous-list.html");
      ASF.Tests.Assert_Contains (T, "List of pages", Reply,
                                 "Wiki list recent page is invalid");
   end Verify_Anonymous;

   --  ------------------------------
   --  Verify that the wiki lists contain the given page.
   --  ------------------------------
   procedure Verify_List_Contains (T    : in out Test;
                                   Name : in String) is
      pragma Unreferenced (Name);

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
   --  Test creation of image by simulating web requests.
   --  ------------------------------
   procedure Test_Create_Image (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Part_Request (1);
      Reply     : Servlet.Responses.Mockup.Response;
      Content   : Ada.Strings.Unbounded.Unbounded_String;
      Folder_Id : ADO.Identifier;
      Image_Id  : ADO.Identifier;
      Path      : constant String := Util.Tests.Get_Test_Path ("regtests/result/upload.jpg");
   begin
      AWA.Tests.Helpers.Users.Login ("test-image@test.com", Request);

      --  Create the folder.
      Request.Set_Parameter ("folder-name", "Image Folder Name");
      Request.Set_Parameter ("storage-folder-create-form", "1");
      Request.Set_Parameter ("storage-folder-create-button", "1");
      ASF.Tests.Do_Post (Request, Reply, "/storages/forms/folder-create.html",
                         "folder-create-form.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_OK,
                "Invalid response after folder creation");

      Reply.Read_Content (Content);
      Folder_Id := AWA.Tests.Helpers.Extract_Identifier (To_String (Content), "#folder");
      T.Assert (Folder_Id > 0, "Invalid folder id returned");

      --  Check the list page.
      ASF.Tests.Do_Get (Request, Reply, "/storages/images.html",
                        "image-list.html");
      ASF.Tests.Assert_Contains (T, "Documents of the workspace", Reply,
                                 "List of documents is invalid (title)");
      ASF.Tests.Assert_Contains (T, "Image Folder Name", Reply,
                                 "List of documents is invalid (content)");

      --  Upload an image to the folder.
      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
      Ada.Directories.Copy_File (Source_Name => "regtests/files/images/Ada-Lovelace.jpg",
                                 Target_Name => Path,
                                 Form        => "all");

      Request.Set_Parameter ("folder", ADO.Identifier'Image (Folder_Id));
      Request.Set_Parameter ("uploadForm", "1");
      Request.Set_Parameter ("id", "-1");
      Request.Set_Parameter ("upload-button", "1");
      Request.Set_Part (Position => 1, Name => "upload-file",
                        Path => Path, Content_Type => "image/jpg");
      ASF.Tests.Do_Post (Request, Reply, "/storages/forms/upload-form.html",
                         "upload-image-form.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_OK,
                "Invalid response after image upload");

      T.Assert_Equals ("application/json", Reply.Get_Content_Type,
                       "Invalid response after upload");

      Reply.Read_Content (Content);
      Image_Id := AWA.Tests.Helpers.Extract_Identifier (To_String (Content), "store");
      T.Assert (Image_Id > 0, "Invalid image id returned after upload");

      --  Look at the image content.
      ASF.Tests.Do_Get (Request, Reply, "/storages/images/"
                          & Util.Strings.Image (Natural (Image_Id)) & "/view/upload.jpg",
                        "image-file-data.jpg");
      T.Assert (Reply.Get_Status = Servlet.Responses.SC_OK,
                "Invalid response after image get");
      T.Assert_Equals ("image/jpg", Reply.Get_Content_Type,
                       "Invalid response after upload");

      --  Look at the image description page.
      ASF.Tests.Do_Get (Request, Reply, "/storages/image-info/"
                          & Util.Strings.Image (Natural (Image_Id)),
                        "image-file-info.html");
      T.Assert (Reply.Get_Status = Servlet.Responses.SC_OK,
                "Invalid response for image-info page");
      T.Assert_Equals ("text/html; charset=UTF-8", Reply.Get_Content_Type,
                       "Invalid response for image-info");
      ASF.Tests.Assert_Contains (T, "/storages/files/"
                                   & Util.Strings.Image (Natural (Image_Id)) & "/", Reply,
                                 "Image info page is invalid (missing link)");

   end Test_Create_Image;

   --  ------------------------------
   --  Test getting an image which does not exist.
   --  ------------------------------
   procedure Test_Missing_Image (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/storages/images/12345345/view/missing.jpg",
                        "image-file-missing.html");
      ASF.Tests.Assert_Redirect (T, "/asfunit/auth/login.html", Reply,
                                "Invalid redirection for protected page");

      AWA.Tests.Helpers.Users.Login ("test-image@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/storages/images/12345345/view/missing.jpg",
                        "image-file-missing.html");
      T.Assert (Reply.Get_Status = Servlet.Responses.SC_NOT_FOUND,
                "Invalid response after image get");
   end Test_Missing_Image;

end AWA.Images.Tests;
