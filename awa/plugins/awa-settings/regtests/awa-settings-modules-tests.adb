-----------------------------------------------------------------------
--  awa-settings-modules-tests -- Unit tests for settings module
--  Copyright (C) 2013, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Measures;

with Security.Contexts;

with AWA.Tests.Helpers.Users;
with AWA.Tests.Helpers.Contexts;
package body AWA.Settings.Modules.Tests is

   package Caller is new Util.Test_Caller (Test, "Settings.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Settings.Get_User_Setting",
                       Test_Get_User_Setting'Access);
      Caller.Add_Test (Suite, "Test AWA.Settings.Set_User_Setting",
                       Test_Set_User_Setting'Access);
      Caller.Add_Test (Suite, "Test AWA.Settings.Get_User_Setting (perf)",
                       Test_Perf_User_Setting'Access);
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
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Tests.Helpers.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-setting@test.com");

      for I in 1 .. 10 loop
         declare
            Name : constant String  := "setting-" & Natural'Image (I);
            R    : Integer;
         begin
            AWA.Settings.Set_User_Setting (Name, I);

            R := AWA.Settings.Get_User_Setting (Name, 0);
            Util.Tests.Assert_Equals (T, I, R, "Invalid Set_User_Setting result");
         end;
      end loop;
   end Test_Set_User_Setting;

   --  ------------------------------
   --  Test performance on user setting.
   --  ------------------------------
   procedure Test_Perf_User_Setting (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Tests.Helpers.Contexts.Service_Context;
      Ident     : constant String := Util.Tests.Get_Uuid;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-setting@test.com");

      --  First pass has to look in the database for the user setting.
      --  Second pass finds the setting in the cache.
      for Pass in 1 .. 2 loop
         declare
            Value : Integer;
            Stamp : Util.Measures.Stamp;
         begin
            for I in 1 .. 100 loop
               Value := AWA.Settings.Get_User_Setting ("perf-" & Integer'Image (I) & Ident, I);
               Util.Tests.Assert_Equals (T, I, Value, "Invalid setting returned");
            end loop;
            Util.Measures.Report (Stamp, "Getting a user setting (100 times) pass"
                                  & Integer'Image (Pass));
         end;
      end loop;

      declare
         Stamp : Util.Measures.Stamp;
      begin
         for I in 1 .. 100 loop
            AWA.Settings.Set_User_Setting ("perf-" & Integer'Image (I) & Ident, I);
         end loop;
         Util.Measures.Report (Stamp, "Saving and creating user setting (100 times)");
      end;

      --  Erase the session cache.
      Context.Session_Attributes.Clear;

      --  Get the user setting that have been created.
      declare
         Value : Integer;
         Stamp : Util.Measures.Stamp;
      begin
         for I in 1 .. 100 loop
            Value := AWA.Settings.Get_User_Setting ("perf-" & Integer'Image (I) & Ident, -1);
            Util.Tests.Assert_Equals (T, I, Value, "Invalid setting returned");
         end loop;
         Util.Measures.Report (Stamp, "Getting a user setting (100 times) load from DB");
      end;
   end Test_Perf_User_Setting;

end AWA.Settings.Modules.Tests;
