-----------------------------------------------------------------------
--  files.tests -- Unit tests for files
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

with Util.Tests;

with AWA.Applications;
with AWA.Tests;
with AWA.Users.Module;
with AWA.Users.Principals;
with ADO.Sessions;
with ADO.SQL;
package body AWA.Users.Services.Tests.Helpers is

   --  ------------------------------
   --  Initialize the service context.
   --  ------------------------------
   procedure Initialize (Principal : in out Test_User) is
   begin
      if Principal.Manager = null then
         Principal.Manager := AWA.Users.Module.Get_User_Manager;
      end if;

      --  Setup the service context.
      Principal.Context.Set_Context (AWA.Tests.Get_Application, null);
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
      Query.Set_Join ("inner join email e on e.user_id = o.id");
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
                                        Principal.User, Principal.Session);
      else
         Principal.Manager.Authenticate (Email    => Email,
                                         Password => "admin",
                                         IpAddr   => "192.168.1.1",
                                         User     => Principal.User,
                                         Session  => Principal.Session);
      end if;
   end Create_User;

   --  ------------------------------
   --  Create a test user for a new test and get an open session.
   --  ------------------------------
   procedure Create_User (Principal : in out Test_User) is
      Key     : AWA.Users.Models.Access_Key_Ref;
	  Email   : constant String := "Joe-" & Util.Tests.Get_UUID & "@gmail.com";
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
                                     Principal.User, Principal.Session);
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
   begin
      Initialize (Principal);

      DB := Principal.Manager.Get_Session;

      --  Find the access key
	  Query.Set_Join ("inner join email e on e.user_id = o.user_id");
      Query.Set_Filter ("e.email = ?");
      Query.Bind_Param (1, Email);
      Key.Find (DB, Query, Found);
   end Find_Access_Key;

   --  ------------------------------
   --  Login a user and create a session
   --  ------------------------------
   procedure Login (Principal : in out Test_User) is
   begin
      Initialize (Principal);
      Principal.Manager.Authenticate (Email    => Principal.Email.Get_Email,
                                      Password => "admin",
                                      IpAddr   => "192.168.1.1",
                                      User     => Principal.User,
                                      Session  => Principal.Session);
   end Login;

   --  ------------------------------
   --  Logout the user and closes the current session.
   --  ------------------------------
   procedure Logout (Principal : in out Test_User) is
   begin
      Initialize (Principal);
      Principal.Manager.Close_Session (Principal.Session.Get_Id);
   end Logout;

   --  Simulate a user login in the given service context.
   procedure Login (Context : in out AWA.Services.Contexts.Service_Context;
                    Sec_Context : in out Security.Contexts.Security_Context;
                    Email   : in String) is
      User      : Test_User;
      Principal : AWA.Users.Principals.Principal_Access;
      App       : AWA.Applications.Application_Access := AWA.Tests.Get_Application;
   begin
      AWA.Tests.Set_Application_Context;
      Create_User (User, Email);
      Principal := AWA.Users.Principals.Create (User.User, User.Session);
      Context.Set_Context (App, Principal);
      Sec_Context.Set_Context (Manager   => App.Get_Permission_Manager,
                               Principal => Principal.all'Access);
   end Login;

end AWA.Users.Services.Tests.Helpers;
