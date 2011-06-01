-----------------------------------------------------------------------
--  users-tests-helpers -- Helpers for user creation
--  Copyright (C) 2011 Stephane Carrez
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

with AWA.Users.Models;
with AWA.Services.Contexts;
package AWA.Users.Services.Tests.Helpers is

   type Test_User is limited record
      Context : AWA.Services.Contexts.Service_Context;
      Manager : User_Service_Access := null;
      User    : AWA.Users.Models.User_Ref;
      Email   : AWA.Users.Models.Email_Ref;
      Session : AWA.Users.Models.Session_Ref;
   end record;

   --  Initialize the service context.
   procedure Initialize (Principal : in out Test_User);

   --  Create a test user for a new test and get an open session.
   procedure Create_User (Principal : in out Test_User);

   --  Login a user and create a session
   procedure Login (Principal : in out Test_User);

   --  Logout the user and closes the current session.
   procedure Logout (Principal : in out Test_User);

end AWA.Users.Services.Tests.Helpers;
