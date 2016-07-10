-----------------------------------------------------------------------
--  awa-storages-services-tests -- Unit tests for storage service
--  Copyright (C) 2012, 2013, 2016 Stephane Carrez
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
package body AWA.Images.Services.Tests is

   use Util.Tests;
   use ADO;

   package Caller is new Util.Test_Caller (Test, "Images.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Images.Create_Image",
                       Test_Create_Image'Access);
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
      T.Manager := AWA.Images.Modules.Get_Image_Manager;

      T.Manager.Create_Thumbnail (Source, Thumb, Width, Height);
      Util.Tests.Assert_Equals (T, 1720, Width, "Invalid image width");
      Util.Tests.Assert_Equals (T, 1098, Height, "Invalid image height");
   end Test_Create_Image;

end AWA.Images.Services.Tests;
