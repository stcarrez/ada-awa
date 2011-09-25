-----------------------------------------------------------------------
--  files.tests -- Unit tests for files
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with Util.Test_Caller;

with ASF.Tests;
with AWA.Tests;
with AWA.Users.Models;
with AWA.Users.Services.Tests.Helpers;

with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Principals;
package body AWA.Users.Tests is

   use ASF.Tests;
   use AWA.Tests;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Users.Tests.Create_User (/users/register.xhtml)",
                       Test_Create_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Close_Session",
                       Test_Logout_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Tests.Login_User (/users/login.xhtml)",
                       Test_Login_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Lost_Password, Reset_Password",
                       Test_Reset_Password_User'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of user by simulating web requests.
   --  ------------------------------
   procedure Test_Create_User (T : in out Test) is
      use type ASF.Principals.Principal_Access;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
	  Email   : constant String := "Joe-" & Util.Tests.Get_UUID & "@gmail.com";
      Principal : Services.Tests.Helpers.Test_User;
   begin
      Services.Tests.Helpers.Initialize (Principal);

      Do_Get (Request, Reply, "/auth/register.html", "create-user-1.html");

      Request.Set_Parameter ("email", Email);
      Request.Set_Parameter ("password", "asdf");
      Request.Set_Parameter ("password2", "asdf");
      Request.Set_Parameter ("firstName", "joe");
      Request.Set_Parameter ("lastName", "dalton");
      Request.Set_Parameter ("register", "1");
      Request.Set_Parameter ("register-button", "1");
      Do_Post (Request, Reply, "/auth/register.html", "create-user-2.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY, "Invalid response");

      --  Check that the user is NOT logged.
      T.Assert (Request.Get_User_Principal = null, "A user principal should not be defined");

	  --  Now, get the access key and simulate a click on the validation link.
	  declare

		 Key       : AWA.Users.Models.Access_Key_Ref;
      begin
	     Services.Tests.Helpers.Find_Access_Key (Principal, Email, Key);
		 T.Assert (not Key.Is_Null, "There is no access key associated with the user");
		 Request.Set_Parameter ("key", Key.Get_Access_Key);
		 Do_Get (Request, Reply, "/auth/validate.html", "validate-user-1.html");

         T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY, "Invalid response");
      end;

      --  Check that the user is logged and we have a user principal now.
      T.Assert (Request.Get_User_Principal /= null, "A user principal should be defined");
   end Test_Create_User;

   procedure Test_Logout_User (T : in out Test) is
   begin
      null;
   end Test_Logout_User;

   --  ------------------------------
   --  Test user authentication by simulating a web request.
   --  ------------------------------
   procedure Test_Login_User (T : in out Test) is
      use type ASF.Principals.Principal_Access;

      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
      Principal :Services.Tests.Helpers.Test_User;
   begin
      AWA.Tests.Set_Application_Context;
      Services.Tests.Helpers.Create_User (Principal, "Joe@gmail.com");

      Do_Get (Request, Reply, "/auth/login.html", "login-user-1.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_OK, "Invalid response");

      --  Check that the user is NOT logged.
      T.Assert (Request.Get_User_Principal = null, "A user principal should not be defined");

      Request.Set_Parameter ("email", "Joe@gmail.com");
      Request.Set_Parameter ("password", "admin");
      Request.Set_Parameter ("login", "1");
      Request.Set_Parameter ("login-button", "1");
      Do_Post (Request, Reply, "/auth/login.html", "login-user-2.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY, "Invalid response");

      --  Check that the user is logged and we have a user principal now.
      T.Assert (Request.Get_User_Principal /= null, "A user principal should be defined");
   end Test_Login_User;

   --  ------------------------------
   --  Test the reset password by simulating web requests.
   --  ------------------------------
   procedure Test_Reset_Password_User (T : in out Test) is
      use type ASF.Principals.Principal_Access;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Email   : constant String := "Joe@gmail.com";
   begin
      Do_Get (Request, Reply, "/auth/lost-password.html", "lost-password-1.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_OK, "Invalid response");

      Request.Set_Parameter ("email", Email);
      Request.Set_Parameter ("lost-password", "1");
      Request.Set_Parameter ("lost-password-button", "1");
      Do_Post (Request, Reply, "/auth/lost-password.html", "lost-password-2.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_OK, "Invalid response");

	  --  Now, get the access key and simulate a click on the reset password link.
	  declare
         Principal : Services.Tests.Helpers.Test_User;
         Key       : AWA.Users.Models.Access_Key_Ref;
      begin
         AWA.Tests.Set_Application_Context;
	     Services.Tests.Helpers.Find_Access_Key (Principal, Email, Key);
		 T.Assert (not Key.Is_Null, "There is no access key associated with the user");

         --  Simulate user clicking on the reset password link.
         --  This verifies the key, login the user and redirect him to the change-password page
         Request.Set_Parameter ("key", Key.Get_Access_Key);
		 Do_Get (Request, Reply, "/auth/reset-password.html", "reset-password-1.html");

         T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY, "Invalid response");

         --  Post the reset password
		 Request.Set_Parameter ("password", "asd");
		 Request.Set_Parameter ("reset-password", "1");
		 Do_Post (Request, Reply, "/auth/change-password.html", "reset-password-2.html");

         T.Assert (Reply.Get_Status = ASF.Responses.SC_OK, "Invalid response");

         --  Check that the user is logged and we have a user principal now.
         t.Assert (Request.Get_User_Principal /= null, "A user principal should be defined");
	 end;
  end Test_Reset_Password_User;

end AWA.Users.Tests;
