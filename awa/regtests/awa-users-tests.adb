-----------------------------------------------------------------------
--  files.tests -- Unit tests for files
--  Copyright (C) 2009, 2010, 2011, 2012, 2017, 2018, 2020 Stephane Carrez
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

with Util.Log;
with Util.Test_Caller;

with ASF.Tests;
with AWA.Users.Models;
with AWA.Tests.Helpers.Users;
with AWA.Users.Services;
with AWA.Tests.Helpers;

with Servlet.Requests.Mockup;
with Servlet.Responses.Mockup;
with Security;
package body AWA.Users.Tests is

   use ASF.Tests;
   use AWA.Tests;
   use type Security.Principal_Access;
   use type AWA.Users.Services.User_Service_Access;

   package Caller is new Util.Test_Caller (Test, "Users.Tests");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Users.Tests.Create_User (/users/register.xhtml)",
                       Test_Create_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Tests.Create_User (Disabled)",
                       Test_Registration_Disabled'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Close_Session",
                       Test_Logout_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Tests.Login_User (/users/login.xhtml)",
                       Test_Login_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Lost_Password, Reset_Password",
                       Test_Reset_Password_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Lost_Password (No user)",
                       Test_Reset_Password_Invalid_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Authenticate",
                       Test_OAuth_Login'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of user by simulating web requests.
   --  ------------------------------
   procedure Test_Create_User (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
      Email     : constant String := "Joe-" & Util.Tests.Get_Uuid & "@gmail.com";
      Principal : AWA.Tests.Helpers.Users.Test_User;
   begin
      AWA.Tests.Helpers.Users.Initialize (Principal);

      Do_Get (Request, Reply, "/auth/register.html", "create-user-1.html");

      Request.Set_Parameter ("email", Email);
      Request.Set_Parameter ("password", "asdf");
      Request.Set_Parameter ("password2", "asdf");
      Request.Set_Parameter ("firstName", "joe");
      Request.Set_Parameter ("lastName", "dalton");
      Request.Set_Parameter ("register", "1");
      Request.Set_Parameter ("register-button", "1");
      Do_Post (Request, Reply, "/auth/register.html", "create-user-2.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_MOVED_TEMPORARILY, "Invalid response");

      --  Check that the user is NOT logged.
      T.Assert (Request.Get_User_Principal = null, "A user principal should not be defined");

      --  Now, get the access key and simulate a click on the validation link.
      declare

         Key       : AWA.Users.Models.Access_Key_Ref;
      begin
         AWA.Tests.Helpers.Users.Find_Access_Key (Principal, Email, Key);
         T.Assert (not Key.Is_Null, "There is no access key associated with the user");

         Do_Get (Request, Reply, "/auth/validate/" & Key.Get_Access_Key,
                 "validate-user-1.html");

         T.Assert (Reply.Get_Status = Servlet.Responses.SC_MOVED_TEMPORARILY, "Invalid response");
      end;

      --  Check that the user is logged and we have a user principal now.
      T.Assert (Request.Get_User_Principal /= null, "A user principal should be defined");
   end Test_Create_User;

   --  ------------------------------
   --  Test creation of user when the registration is disabled.
   --  ------------------------------
   procedure Test_Registration_Disabled (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
      Email     : constant String := "Joe-" & Util.Tests.Get_Uuid & "@gmail.com";
      Principal : AWA.Tests.Helpers.Users.Test_User;
   begin
      AWA.Tests.Helpers.Users.Initialize (Principal);

      T.Assert (Principal.Manager /= null, "No User Manager defined");
      Principal.Manager.Set_Allow_Register (False);
      Do_Get (Request, Reply, "/auth/register.html", "register-disabled-1.html");

      Request.Set_Parameter ("email", Email);
      Request.Set_Parameter ("password", "asdf");
      Request.Set_Parameter ("password2", "asdf");
      Request.Set_Parameter ("firstName", "joe");
      Request.Set_Parameter ("lastName", "dalton");
      Request.Set_Parameter ("register", "1");
      Request.Set_Parameter ("register-button", "1");
      Do_Post (Request, Reply, "/auth/register.html", "register-disabled-2.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_MOVED_TEMPORARILY, "Invalid response");

      --  Check that the user is NOT logged.
      T.Assert (Request.Get_User_Principal = null, "A user principal should not be defined");

      --  Check that we are redirected to the /auth/login.html page with the
      --  user registration disabled message.
      ASF.Tests.Assert_Redirect (T, "/asfunit/auth/login.html",
                                 Reply, "Invalid redirection when registration disabled");

      Do_Get (Request, Reply, "/auth/login.html",
              "register-disabled-3.html");

      ASF.Tests.Assert_Matches (T, "The user registration is disabled on this server.",
                                Reply, "Invalid error page after user creation (disabled)",
                                Servlet.Responses.SC_OK);

      --  Second test using the OAuth2 fake provider.
      Do_Get (Request, Reply, "/auth/auth/google", "oauth-google.html");
      T.Assert (Reply.Get_Status = Servlet.Responses.SC_MOVED_TEMPORARILY, "Invalid response");

      Request.Set_Parameter ("email", "oauth-user-unreg@fake.com");
      Request.Set_Parameter ("id", "oauth-user-id-not-registered-fake");
      Request.Set_Parameter ("claimed_id", "oauth-user-unreg-claimed_id-fake");
      Do_Get (Request, Reply, "/auth/verify?email=oauth-user-unreg@fake.com&"
              & "id=oauth-user-id-not-registered-fake&claimed_id=oauth-user-unreg-claimed_id-fake",
              "oauth-verify-unreg.html");

      --  Check that the user is NOT logged.
      T.Assert (Request.Get_User_Principal = null, "A user principal must not be defined");

      ASF.Tests.Assert_Redirect (T, "/asfunit/auth/login.html",
                                 Reply, "Invalid redirection when registration disabled");

      Do_Get (Request, Reply, "/auth/login.html",
              "register-disabled-4.html");

      ASF.Tests.Assert_Matches (T, "The user registration is disabled on this server.",
                                Reply, "Invalid error page after user creation (disabled)",
                                Servlet.Responses.SC_OK);

      --  Restore user registration for other tests.
      Principal.Manager.Set_Allow_Register (True);

   exception
      when others =>
         Principal.Manager.Set_Allow_Register (True);
         raise;

   end Test_Registration_Disabled;

   procedure Test_Logout_User (T : in out Test) is
   begin
      null;
   end Test_Logout_User;

   --  ------------------------------
   --  Test user authentication by simulating a web request.
   --  ------------------------------
   procedure Test_Login_User (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
      Principal : AWA.Tests.Helpers.Users.Test_User;
   begin
      AWA.Tests.Set_Application_Context;
      AWA.Tests.Get_Application.Dump_Routes (Util.Log.INFO_LEVEL);
      begin
         AWA.Tests.Helpers.Users.Create_User (Principal, "Joe@gmail.com");

         --  It sometimes happen that Joe's password is changed by another test.
         --  Recover the password and make sure it is 'admin'.
      exception
         when others =>
            T.Recover_Password ("Joe@gmail.com", "admin");
      end;

      Do_Get (Request, Reply, "/auth/login.html", "login-user-1.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_OK, "Invalid response");

      --  Check that the user is NOT logged.
      T.Assert (Request.Get_User_Principal = null, "A user principal should not be defined");

      Request.Set_Parameter ("email", "Joe@gmail.com");
      Request.Set_Parameter ("password", "admin");
      Request.Set_Parameter ("login", "1");
      Request.Set_Parameter ("login-button", "1");
      Do_Post (Request, Reply, "/auth/login.html", "login-user-2.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_MOVED_TEMPORARILY, "Invalid response");

      --  Check that the user is logged and we have a user principal now.
      T.Assert (Request.Get_User_Principal /= null, "A user principal should be defined");
   end Test_Login_User;

   --  ------------------------------
   --  Test OAuth access using a fake OAuth provider.
   --  ------------------------------
   procedure Test_OAuth_Login (T : in out Test) is
      Request   : Servlet.Requests.Mockup.Request;
      Reply     : Servlet.Responses.Mockup.Response;
   begin
      Do_Get (Request, Reply, "/auth/auth/google", "oauth-google.html");
      T.Assert (Reply.Get_Status = Servlet.Responses.SC_MOVED_TEMPORARILY, "Invalid response");

      Request.Set_Parameter ("email", "oauth-user@fake.com");
      Request.Set_Parameter ("id", "oauth-user-id-fake");
      Request.Set_Parameter ("claimed_id", "oauth-user-claimed_id-fake");
      Do_Get (Request, Reply, "/auth/verify?email=oauth-user@fake.com&"
              & "id=oauth-user-id-fake&claimed_id=oauth-user-claimed-id-fake",
              "oauth-verify.html");
      T.Assert (Reply.Get_Status = Servlet.Responses.SC_MOVED_TEMPORARILY, "Invalid response");

      --  Check that the user is logged and we have a user principal now.
      T.Assert (Request.Get_User_Principal /= null, "A user principal should be defined");
      Util.Tests.Assert_Equals (T, "Oauth-user", Request.Get_User_Principal.Get_Name,
                                "Invalid user name after OAuth authentication");
   end Test_OAuth_Login;

   --  ------------------------------
   --  Run the recovery password process for the given user and change the password.
   --  ------------------------------
   procedure Recover_Password (T : in out Test;
                               Email : in String;
                               Password : in String) is
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Do_Get (Request, Reply, "/auth/lost-password.html", "lost-password-1.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_OK, "Invalid response");

      Request.Set_Parameter ("email", Email);
      Request.Set_Parameter ("lost-password", "1");
      Request.Set_Parameter ("lost-password-button", "1");
      Do_Post (Request, Reply, "/auth/lost-password.html", "lost-password-2.html");

      ASF.Tests.Assert_Redirect (T, "/asfunit/auth/login.html",
                                 Reply, "Invalid redirect after lost password");

      Do_Get (Request, Reply, "/auth/login.html", "lost-password-3.html");

      ASF.Tests.Assert_Matches (T, "A message was sent to your email address.",
                                Reply, "Invalid message after lost password",
                                Servlet.Responses.SC_OK);

      --  Now, get the access key and simulate a click on the reset password link.
      declare
         Principal : AWA.Tests.Helpers.Users.Test_User;
         Key       : AWA.Users.Models.Access_Key_Ref;
      begin
         AWA.Tests.Set_Application_Context;
         AWA.Tests.Helpers.Users.Find_Access_Key (Principal, Email, Key);
         T.Assert (not Key.Is_Null, "There is no access key associated with the user");

         --  Simulate user clicking on the reset password link.
         --  This verifies the key, login the user and redirect him to the change-password page
         Request.Set_Parameter ("key", Key.Get_Access_Key);
         Request.Set_Parameter ("password", Password);
         Request.Set_Parameter ("reset-password", "1");
         Do_Post (Request, Reply, "/auth/change-password/" & Key.Get_Access_Key,
                  "recover-password-2.html");

         ASF.Tests.Assert_Redirect (T, "/asfunit/workspaces/main.html",
                                    Reply, "Invalid redirect after lost password");

         --  Check that the user is logged and we have a user principal now.
         T.Assert (Request.Get_User_Principal /= null, "A user principal should be defined");
      end;
   end Recover_Password;

   --  ------------------------------
   --  Test the reset password by simulating web requests.
   --  ------------------------------
   procedure Test_Reset_Password_User (T : in out Test) is
   begin
      T.Recover_Password ("Joe@gmail.com", "asf");
      T.Recover_Password ("Joe@gmail.com", "admin");
   end Test_Reset_Password_User;

   --  ------------------------------
   --  Test the reset password with an invalid user.
   --  ------------------------------
   procedure Test_Reset_Password_Invalid_User (T : in out Test) is
      Request : Servlet.Requests.Mockup.Request;
      Reply   : Servlet.Responses.Mockup.Response;
   begin
      Do_Get (Request, Reply, "/auth/lost-password.html", "lost-password-4.html");

      T.Assert (Reply.Get_Status = Servlet.Responses.SC_OK, "Invalid response");

      Request.Set_Parameter ("email", "voldemort@gmail.com");
      Request.Set_Parameter ("lost-password", "1");
      Request.Set_Parameter ("lost-password-button", "1");
      Do_Post (Request, Reply, "/auth/lost-password.html", "lost-password-5.html");

      ASF.Tests.Assert_Redirect (T, "/asfunit/auth/login.html",
                                 Reply, "Invalid redirect after lost password");

   end Test_Reset_Password_Invalid_User;

end AWA.Users.Tests;
