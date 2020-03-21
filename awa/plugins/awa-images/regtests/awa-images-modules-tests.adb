-----------------------------------------------------------------------
--  awa-storages-modules-tests -- Unit tests for storage service
--  Copyright (C) 2012, 2013, 2016, 2019, 2020 Stephane Carrez
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

with Security.Contexts;

with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with AWA.Images.Modules;
package body AWA.Images.Modules.Tests is

   package Caller is new Util.Test_Caller (Test, "Images.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Images.Modules.Create_Image",
                       Test_Create_Image'Access);
      Caller.Add_Test (Suite, "Test AWA.Images.Modules.Get_Sizes",
                       Test_Get_Sizes'Access);
      Caller.Add_Test (Suite, "Test AWA.Images.Modules.Scale",
                       Test_Scale'Access);
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
      Util.Tests.Assert_Equals (T, 0, Width, "Invalid width");
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

end AWA.Images.Modules.Tests;
