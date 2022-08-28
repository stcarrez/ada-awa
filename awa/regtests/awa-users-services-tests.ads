-----------------------------------------------------------------------
--  awa-users-services-tests -- Unit tests for user service
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
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

package AWA.Users.Services.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   procedure Test_Create_User (T : in out Test);
   procedure Test_Logout_User (T : in out Test);
   procedure Test_Login_User (T : in out Test);
   procedure Test_Reset_Password_User (T : in out Test);
   procedure Test_Get_Module (T : in out Test);

   --  Disable a user and check login is refused.
   procedure Test_Disable_User (T : in out Test);

end AWA.Users.Services.Tests;
