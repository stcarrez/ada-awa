-----------------------------------------------------------------------
--  users-tests-helpers -- Helpers for user creation
--  Copyright (C) 2011, 2012, 2013, 2014, 2017, 2020 Stephane Carrez
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
with Ada.Finalization;

with Security.Contexts;
with ASF.Requests.Mockup;
with AWA.Users.Models;
with AWA.Users.Services;
with AWA.Services.Contexts;
with AWA.Users.Principals;
package AWA.Tests.Helpers.Users is

   type Test_User is new Ada.Finalization.Limited_Controlled with record
      Context   : AWA.Services.Contexts.Service_Context;
      Manager   : AWA.Users.Services.User_Service_Access := null;
      User      : AWA.Users.Models.User_Ref;
      Email     : AWA.Users.Models.Email_Ref;
      Session   : AWA.Users.Models.Session_Ref;
      Principal : AWA.Users.Principals.Principal_Access;
   end record;

   --  Initialize the service context.
   procedure Initialize (Principal : in out Test_User);

   --  Create a test user associated with the given email address.
   --  Get an open session for that user.  If the user already exists, no error is reported.
   procedure Create_User (Principal : in out Test_User;
                          Email     : in String);

   --  Create a test user for a new test and get an open session.
   procedure Create_User (Principal : in out Test_User);

   --  Find the access key associated with a user (if any).
   procedure Find_Access_Key (Principal : in out Test_User;
                              Email     : in String;
                              Key       : in out AWA.Users.Models.Access_Key_Ref);

   --  Login a user and create a session
   procedure Login (Principal : in out Test_User);

   --  Logout the user and closes the current session.
   procedure Logout (Principal : in out Test_User);

   --  Simulate a user login in the given service context.
   procedure Login (Context     : in out AWA.Services.Contexts.Service_Context'Class;
                    Sec_Context : in out Security.Contexts.Security_Context;
                    Email       : in String);

   --  Simulate a user login on the request.  Upon successful login, a session that is
   --  authentified is associated with the request object.
   procedure Login (Email   : in String;
                    Request : in out ASF.Requests.Mockup.Request'Class);

   --  Setup the context and security context to simulate an anonymous user.
   procedure Anonymous (Context     : in out AWA.Services.Contexts.Service_Context'Class;
                        Sec_Context : in out Security.Contexts.Security_Context);

   --  Simulate the recovery password process for the given user.
   procedure Recover_Password (Email : in String);

   overriding
   procedure Finalize (Principal : in out Test_User);

   --  Cleanup and release the Principal that have been allocated from the Login session
   --  but not released because the Logout is not called from the unit test.
   procedure Tear_Down;

end AWA.Tests.Helpers.Users;
