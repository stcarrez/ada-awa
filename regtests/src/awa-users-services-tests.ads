-----------------------------------------------------------------------
--  awa-users-services-tests -- Unit tests for user service
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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

   --  Create a user and try to login without the verify.
   procedure Test_Create_User_No_Verify (T : in out Test);

end AWA.Users.Services.Tests;
