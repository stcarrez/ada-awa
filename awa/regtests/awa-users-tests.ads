-----------------------------------------------------------------------
--  awa-users-tests -- Unit tests for AWA users
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2020, 2022 Stephane Carrez
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

package AWA.Users.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   --  Test creation of user by simulating web requests.
   procedure Test_Create_User (T : in out Test);

   --  Test creation of user when the registration is disabled.
   procedure Test_Registration_Disabled (T : in out Test);

   procedure Test_Logout_User (T : in out Test);

   --  Test user authentication by simulating a web request.
   procedure Test_Login_User (T : in out Test);

   --  Test the reset password by simulating web requests.
   procedure Test_Reset_Password_User (T : in out Test);

   --  Test OAuth access using a fake OAuth provider.
   procedure Test_OAuth_Login (T : in out Test);

   --  Run the recovery password process for the given user and change the password.
   procedure Recover_Password (T        : in out Test;
                               Email    : in String;
                               Password : in String);

end AWA.Users.Tests;
