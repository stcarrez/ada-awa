-----------------------------------------------------------------------
--  awa-storages-modules-tests -- Unit tests for storage service
--  Copyright (C) 2012, 2013, 2016, 2019, 2020, 2022 Stephane Carrez
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

with Security.Contexts;

with ASF.Tests;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with Servlet.Responses;

with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with AWA.Storages.Beans;
with AWA.Storages.Services;
with AWA.Storages.Modules;
package body AWA.Images.Modules.Tests is

   use type AWA.Storages.Services.Storage_Service_Access;

   package Caller is new Util.Test_Caller (Test, "Images.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Images.Modules.Create_Image",
                       Test_Create_Image'Access);
      Caller.Add_Test (Suite, "Test AWA.Images.Modules.Get_Sizes",
                       Test_Get_Sizes'Access);
      Caller.Add_Test (Suite, "Test AWA.Images.Modules.Scale",
                       Test_Scale'Access);
      Caller.Add_Test (Suite, "Test AWA.Images.Modules.On_Create",
                       Test_Store_Image'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of a storage object
   --  ------------------------------
   procedure Test_Create_Image (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Source    : constant String := Util.Tests.Get_Path ("regtests/files/images/bast-12.jpg");
      Thumb     : constant String
        := Util.Tests.Get_Test_Path ("regtests/result/bast-12-thumb.jpg");
      Width     : Natural := 64;
      Height    : Natural := 64;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-storage@test.com");
      T.Manager := AWA.Images.Modules.Get_Image_Module;

      T.Manager.Create_Thumbnail (Source, Thumb, Width, Height);
      Util.Tests.Assert_Equals (T, 1720, Width, "Invalid image width");
      Util.Tests.Assert_Equals (T, 1098, Height, "Invalid image height");
   end Test_Create_Image;

   --  ------------------------------
   --  Test the Get_Sizes operation.
   --  ------------------------------
   procedure Test_Get_Sizes (T : in out Test) is
      Width  : Natural;
      Height : Natural;
   begin
      AWA.Images.Modules.Get_Sizes ("default", Width, Height);
      Util.Tests.Assert_Equals (T, 800, Width, "Default width should be 800");
      Util.Tests.Assert_Equals (T, 0, Height, "Default height should be 0");

      AWA.Images.Modules.Get_Sizes ("123x456", Width, Height);
      Util.Tests.Assert_Equals (T, 123, Width, "Invalid width");
      Util.Tests.Assert_Equals (T, 456, Height, "Invalid height");

      AWA.Images.Modules.Get_Sizes ("x56", Width, Height);
      Util.Tests.Assert_Equals (T, 0, Width, "Invalid width");
      Util.Tests.Assert_Equals (T, 56, Height, "Invalid height");

      AWA.Images.Modules.Get_Sizes ("123x", Width, Height);
      Util.Tests.Assert_Equals (T, 123, Width, "Invalid width");
      Util.Tests.Assert_Equals (T, 0, Height, "Invalid height");

      AWA.Images.Modules.Get_Sizes ("123xtoto", Width, Height);
      Util.Tests.Assert_Equals (T, 123, Width, "Invalid width");
      Util.Tests.Assert_Equals (T, 0, Height, "Invalid height");

      AWA.Images.Modules.Get_Sizes ("xtoto", Width, Height);
      Util.Tests.Assert_Equals (T, 0, Width, "Invalid width");
      Util.Tests.Assert_Equals (T, 0, Height, "Invalid height");

      AWA.Images.Modules.Get_Sizes ("original", Width, Height);
      Util.Tests.Assert_Equals (T, Natural'Last, Width, "Invalid width");
      Util.Tests.Assert_Equals (T, Natural'Last, Height, "Invalid height");
   end Test_Get_Sizes;

   --  ------------------------------
   --  Test the Scale operation.
   --  ------------------------------
   procedure Test_Scale (T : in out Test) is
      Width  : Natural;
      Height : Natural;
   begin
      Width := 0;
      Height := 0;
      AWA.Images.Modules.Scale (123, 456, Width, Height);
      Util.Tests.Assert_Equals (T, 123, Width, "Invalid width");
      Util.Tests.Assert_Equals (T, 456, Height, "Invalid height");

      Width := 100;
      Height := 0;
      AWA.Images.Modules.Scale (10000, 2000, Width, Height);
      Util.Tests.Assert_Equals (T, 100, Width, "Invalid width");
      Util.Tests.Assert_Equals (T, 20, Height, "Invalid height");

      Width := 0;
      Height := 200;
      AWA.Images.Modules.Scale (10000, 2000, Width, Height);
      Util.Tests.Assert_Equals (T, 1000, Width, "Invalid width");
      Util.Tests.Assert_Equals (T, 200, Height, "Invalid height");

   end Test_Scale;

   --  ------------------------------
   --  Test the creation of an image through the storage service.
   --  ------------------------------
   procedure Test_Store_Image (T : in out Test) is
      Sec_Ctx : Security.Contexts.Security_Context;
      Context : AWA.Services.Contexts.Service_Context;
      Folder  : AWA.Storages.Beans.Folder_Bean;
      Store   : AWA.Storages.Models.Storage_Ref;
      Mgr     : AWA.Storages.Services.Storage_Service_Access;
      Outcome : Ada.Strings.Unbounded.Unbounded_String;
      Path    : constant String
        := Util.Tests.Get_Path ("regtests/files/images/Ada-Lovelace.jpg");
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-storage@test.com");

      Mgr := AWA.Storages.Modules.Get_Storage_Manager;
      T.Assert (Mgr /= null, "Null storage manager");

      --  Make a storage folder.
      Folder.Module := AWA.Storages.Modules.Get_Storage_Module;
      Folder.Set_Name ("Image folder");
      Folder.Save (Outcome);

      Store.Set_Folder (Folder);
      Store.Set_Mime_Type ("image/jpg");
      Store.Set_Name ("Ada-Lovelace.jpg");
      Mgr.Save (Store, Path, AWA.Storages.Models.FILE);

      declare
         Request : ASF.Requests.Mockup.Request;
         Reply   : ASF.Responses.Mockup.Response;
         Id      : constant String := ADO.Identifier'Image (Store.Get_Id);
      begin
         AWA.Tests.Helpers.Users.Login ("test-storage@test.com", Request);
         ASF.Tests.Do_Get (Request, Reply,
                           "/storages/images/" & Id (Id'First + 1 .. Id'Last)
                           & "/view/Ada-Lovelace.jpg",
                           "image-get-Ada-Lovelace.jpg");
         ASF.Tests.Assert_Header (T, "Content-Type", "image/jpg", Reply);
         Util.Tests.Assert_Equals (T, Servlet.Responses.SC_OK,
                                   Reply.Get_Status,
                                   "Invalid response for image");

         --  Try to get an invalid image
         ASF.Tests.Do_Get (Request, Reply,
                           "/storages/images/plop"
                           & "/view/Ada-Lovelace.jpg",
                           "image-get-plop.jpg");
         Util.Tests.Assert_Equals (T, Servlet.Responses.SC_NOT_FOUND,
                                   Reply.Get_Status,
                                   "Invalid response for image");

      end;
   end Test_Store_Image;

end AWA.Images.Modules.Tests;
