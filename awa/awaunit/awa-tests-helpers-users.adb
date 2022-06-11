-----------------------------------------------------------------------
--  files.tests -- Unit tests for files
--  Copyright (C) 2011, 2012, 2013, 2014, 2017, 2020, 2022 Stephane Carrez
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
with Ada.Unchecked_Deallocation;

with Util.Tests;
with Util.Log.Loggers;

with ASF.Principals;
with ASF.Tests;
with ASF.Responses.Mockup;
with AWA.Applications;
with AWA.Users.Modules;
with ADO.Sessions;
with ADO.SQL;
package body AWA.Tests.Helpers.Users is

   use AWA.Users.Services;
   use type AWA.Users.Principals.Principal_Access;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Tests.Helpers.Users");

   MAX_USERS    : constant Positive := 10;
   Logged_Users : array (1 .. MAX_USERS) of AWA.Users.Principals.Principal_Access;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => AWA.Users.Principals.Principal'Class,
                                     Name   => AWA.Users.Principals.Principal_Access);

   --  ------------------------------
   --  Initialize the service context.
   --  ------------------------------
   procedure Initialize (Principal : in out Test_User) is
   begin
      --  Setup the service context.
      Principal.Context.Set_Context (AWA.Tests.Get_Application, null);

      if Principal.Manager = null then
         Principal.Manager := AWA.Users.Modules.Get_User_Manager;
         if Principal.Manager = null then
            Log.Error ("There is no User_Manager in the application.");
         end if;
      end if;
   end Initialize;

   --  ------------------------------
   --  Create a test user associated with the given email address.
   --  Get an open session for that user.  If the user already exists, no error is reported.
   --  ------------------------------
   procedure Create_User (Principal : in out Test_User;
                          Email     : in String) is
      DB    : ADO.Sessions.Session;
      Query : ADO.SQL.Query;
      Found : Boolean;
      Key   : AWA.Users.Models.Access_Key_Ref;
   begin
      Initialize (Principal);

      DB := Principal.Manager.Get_Session;

      --  Find the user
      Query.Set_Join ("inner join awa_email e on e.user_id = o.id");
      Query.Set_Filter ("e.email = ?");
      Query.Bind_Param (1, Email);
      Principal.User.Find (DB, Query, Found);
      if not Found then
         Principal.User.Set_First_Name ("Joe");
         Principal.User.Set_Last_Name ("Pot");
         Principal.User.Set_Password ("admin");
         Principal.Email.Set_Email (Email);
         Principal.Manager.Create_User (Principal.User, Principal.Email);

         Find_Access_Key (Principal, Email, Key);

         --  Run the verification and get the user and its session
         Principal.Manager.Verify_User (Key.Get_Access_Key, "192.168.1.1",
                                        Principal.Principal);
      else
         Principal.Manager.Authenticate (Email     => Email,
                                         Password  => "admin",
                                         IpAddr    => "192.168.1.1",
                                         Principal => Principal.Principal);
      end if;
      Principal.User    := Principal.Principal.Get_User;
      Principal.Session := Principal.Principal.Get_Session;
   end Create_User;

   --  ------------------------------
   --  Create a test user for a new test and get an open session.
   --  ------------------------------
   procedure Create_User (Principal : in out Test_User) is
      Key     : AWA.Users.Models.Access_Key_Ref;
      Email   : constant String := "Joe-" & Util.Tests.Get_Uuid & "@gmail.com";
   begin
      Initialize (Principal);
      Principal.User.Set_First_Name ("Joe");
      Principal.User.Set_Last_Name ("Pot");
      Principal.User.Set_Password ("admin");
      Principal.Email.Set_Email (Email);
      Principal.Manager.Create_User (Principal.User, Principal.Email);

      Find_Access_Key (Principal, Email, Key);

      --  Run the verification and get the user and its session
      Principal.Manager.Verify_User (Key.Get_Access_Key, "192.168.1.1",
                                     Principal.Principal);
      Principal.User    := Principal.Principal.Get_User;
      Principal.Session := Principal.Principal.Get_Session;
   end Create_User;

   --  ------------------------------
   --  Find the access key associated with a user (if any).
   --  ------------------------------
   procedure Find_Access_Key (Principal : in out Test_User;
                              Email     : in String;
                              Key       : in out AWA.Users.Models.Access_Key_Ref) is
      DB    : ADO.Sessions.Session;
      Query : ADO.SQL.Query;
      Found : Boolean;
      Signup_Kind  : constant Integer
        := AWA.Users.Models.Key_Type'Pos (AWA.Users.Models.SIGNUP_KEY);
      Password_Kind  : constant Integer
        := AWA.Users.Models.Key_Type'Pos (AWA.Users.Models.RESET_PASSWORD_KEY);
   begin
      Initialize (Principal);

      DB := Principal.Manager.Get_Session;

      --  Find the access key
      Query.Set_Join ("inner join awa_email e on e.user_id = o.user_id");
      Query.Set_Filter ("e.email = ? AND o.kind = ?");
      Query.Bind_Param (1, Email);
      Query.Bind_Param (2, Signup_Kind);
      Key.Find (DB, Query, Found);
      if not Found then
         Query.Bind_Param (2, Password_Kind);
         Key.Find (DB, Query, Found);
         if not Found then
            Log.Error ("Cannot find access key for email {0}", Email);
         end if;
      end if;
   end Find_Access_Key;

   --  ------------------------------
   --  Login a user and create a session
   --  ------------------------------
   procedure Login (Principal : in out Test_User) is
   begin
      Initialize (Principal);
      Principal.Manager.Authenticate (Email     => Principal.Email.Get_Email,
                                      Password  => "admin",
                                      IpAddr    => "192.168.1.1",
                                      Principal => Principal.Principal);
      Principal.User    := Principal.Principal.Get_User;
      Principal.Session := Principal.Principal.Get_Session;
   end Login;

   --  ------------------------------
   --  Logout the user and closes the current session.
   --  ------------------------------
   procedure Logout (Principal : in out Test_User) is
   begin
      Initialize (Principal);
      Principal.Manager.Close_Session (Principal.Session.Get_Id, True);
   end Logout;

   --  ------------------------------
   --  Simulate a user login in the given service context.
   --  ------------------------------
   procedure Login (Context     : in out AWA.Services.Contexts.Service_Context'Class;
                    Sec_Context : in out Security.Contexts.Security_Context;
                    Email       : in String) is
      User      : Test_User;
      Principal : AWA.Users.Principals.Principal_Access;
      App       : constant AWA.Applications.Application_Access := AWA.Tests.Get_Application;
   begin
      AWA.Tests.Set_Application_Context;
      Create_User (User, Email);
      Principal := AWA.Users.Principals.Create (User.User, User.Session);
      Context.Set_Context (App, Principal);
      Sec_Context.Set_Context (Manager   => App.Get_Security_Manager,
                               Principal => Principal.all'Access);

      --  Keep track of the Principal instance so that Tear_Down will release it.
      --  Most tests will call Login but don't call Logout because there is no real purpose
      --  for the test in doing that and it allows to keep the unit test simple.  This creates
      --  memory leak because the Principal instance is not freed.
      for I in Logged_Users'Range loop
         if Logged_Users (I) = null then
            Logged_Users (I) := Principal;
            exit;
         end if;
      end loop;
   end Login;

   --  ------------------------------
   --  Simulate a user login on the request.  Upon successful login, a session that is
   --  authentified is associated with the request object.
   --  ------------------------------
   procedure Login (Email   : in String;
                    Request : in out ASF.Requests.Mockup.Request'Class) is
      User  : Test_User;
      Reply : ASF.Responses.Mockup.Response;
   begin
      Create_User (User, Email);
      ASF.Tests.Do_Get (Request, Reply, "/auth/login.html", "login-user-1.html");

      Request.Set_Parameter ("email", Email);
      Request.Set_Parameter ("password", "admin");
      Request.Set_Parameter ("login", "1");
      Request.Set_Parameter ("login-button", "1");
      ASF.Tests.Do_Post (Request, Reply, "/auth/login.html", "login-user-2.html");
   end Login;

   --  ------------------------------
   --  Setup the context and security context to simulate an anonymous user.
   --  ------------------------------
   procedure Anonymous (Context     : in out AWA.Services.Contexts.Service_Context'Class;
                        Sec_Context : in out Security.Contexts.Security_Context) is
      App : constant AWA.Applications.Application_Access := AWA.Tests.Get_Application;
   begin
      AWA.Tests.Set_Application_Context;
      Context.Set_Context (App, null);
      Sec_Context.Set_Context (Manager   => App.Get_Security_Manager,
                               Principal => null);
   end Anonymous;

   --  ------------------------------
   --  Simulate the recovery password process for the given user.
   --  ------------------------------
   procedure Recover_Password (Email : in String) is
      use type ASF.Principals.Principal_Access;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
   begin
      Request.Set_Parameter ("email", Email);
      Request.Set_Parameter ("lost-password", "1");
      Request.Set_Parameter ("lost-password-button", "1");
      ASF.Tests.Do_Post (Request, Reply, "/auth/lost-password.html", "lost-password-2.html");

      if Reply.Get_Status /= ASF.Responses.SC_MOVED_TEMPORARILY then
         Log.Error ("Invalid redirect after lost password");
      end if;

      --  Now, get the access key and simulate a click on the reset password link.
      declare
         Principal : AWA.Tests.Helpers.Users.Test_User;
         Key       : AWA.Users.Models.Access_Key_Ref;
      begin
         AWA.Tests.Set_Application_Context;
         AWA.Tests.Helpers.Users.Find_Access_Key (Principal, Email, Key);
         if not Key.Is_Null then
            Log.Error ("There is no access key associated with the user");
         end if;

         --  Simulate user clicking on the reset password link.
         --  This verifies the key, login the user and redirect him to the change-password page
         Request.Set_Parameter ("key", Key.Get_Access_Key);
         Request.Set_Parameter ("password", "admin");
         Request.Set_Parameter ("reset-password", "1");
         ASF.Tests.Do_Post (Request, Reply, "/auth/change-password.html", "reset-password-2.html");

         if Reply.Get_Status /= ASF.Responses.SC_MOVED_TEMPORARILY then
            Log.Error ("Invalid response");
         end if;

         --  Check that the user is logged and we have a user principal now.
         if Request.Get_User_Principal = null then
            Log.Error ("A user principal should be defined");
         end if;
      end;
   end Recover_Password;

   overriding
   procedure Finalize (Principal : in out Test_User) is
   begin
      Free (Principal.Principal);
   end Finalize;

   --  ------------------------------
   --  Cleanup and release the Principal that have been allocated from the Login session
   --  but not released because the Logout is not called from the unit test.
   --  ------------------------------
   procedure Tear_Down is
   begin
      for I in Logged_Users'Range loop
         if Logged_Users (I) /= null then
            Free (Logged_Users (I));
         end if;
      end loop;
   end Tear_Down;

end AWA.Tests.Helpers.Users;
