-----------------------------------------------------------------------
--  awa-settings-modules-tests -- Unit tests for settings module
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Settings.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   --  Test getting a user setting.
   procedure Test_Get_User_Setting (T : in out Test);

   --  Test saving a user setting.
   procedure Test_Set_User_Setting (T : in out Test);

   --  Test performance on user setting.
   procedure Test_Perf_User_Setting (T : in out Test);

end AWA.Settings.Modules.Tests;
