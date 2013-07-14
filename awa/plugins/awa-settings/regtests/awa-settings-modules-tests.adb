-----------------------------------------------------------------------
--  awa-settings-modules-tests -- Unit tests for settings module
--  Copyright (C) 2013 Stephane Carrez
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
with Util.Beans.Basic;
with Util.Beans.Objects;

with ADO;
with Security.Contexts;

with ASF.Contexts.Faces;
with ASF.Contexts.Faces.Mockup;

with AWA.Permissions;
with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with AWA.Tests.Helpers.Contexts;
package body AWA.Settings.Modules.Tests is

   use Util.Tests;
   use ADO;

   package Caller is new Util.Test_Caller (Test, "Questions.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Settings.Get_User_Setting",
                       Test_Get_User_Setting'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test getting a user setting.
   --  ------------------------------
   procedure Test_Get_User_Setting (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Tests.Helpers.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-setting@test.com");

      for I in 1 .. 10 loop
         declare
            Name : constant String  := "setting-" & Natural'Image (I);
            R    : constant Integer := AWA.Settings.Get_User_Setting (Name, I);
         begin
            Util.Tests.Assert_Equals (T, I, R, "Invalid Get_User_Setting result");
         end;
      end loop;
   end Test_Get_User_Setting;

   --  ------------------------------
   --  Test saving a user setting.
   --  ------------------------------
   procedure Test_Set_User_Setting (T : in out Test) is
   begin
      null;
   end Test_Set_User_Setting;

end AWA.Settings.Modules.Tests;
