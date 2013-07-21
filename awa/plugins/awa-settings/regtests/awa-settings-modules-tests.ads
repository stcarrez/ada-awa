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
