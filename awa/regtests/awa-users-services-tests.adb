-----------------------------------------------------------------------
--  users - User creation, password tests
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

with Util.Test_Caller;
with Util.Measures;

with ADO;
with ADO.Sessions;
with ADO.SQL;
with ADO.Objects;
with Ada.Calendar;

with AWA.Users.Module;
with AWA.Users.Services.Tests.Helpers;
package body AWA.Users.Services.Tests is

   use Util.Tests;
   use ADO;
   use ADO.Objects;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Create_User",
                       Test_Create_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Close_Session",
                       Test_Logout_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Authenticate, Close_Session",
                       Test_Login_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Lost_Password, Reset_Password",
                       Test_Reset_Password_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Module.Get_User_Module",
                       Test_Get_Module'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of a user
   --  ------------------------------
   procedure Test_Create_User (T : in out Test) is
      Principal : Helpers.Test_User;
   begin
      --  Create the user
      Helpers.Create_User (Principal);
      T.Assert (not Principal.User.Is_Null, "User is created");
      T.Assert (Principal.User.Get_Id > 0, "User has an allocated key");
      T.Assert (Principal.Email.Get_Id > 0, "Email has an allocated key");
      T.Assert (not Principal.Session.Is_Null, "Session must be created");
      T.Assert (Principal.Session.Get_End_Date.Is_Null, "Session must be opened");

      --  Verify the user session
      declare
         S1 : Session_Ref;
         U1 : User_Ref;
      begin
         Principal.Manager.Verify_Session (Id => Principal.Session.Get_Id,
                                           Session => S1, User => U1);

         t.Assert (not S1.Is_Null, "Null session returned by Verify_Session");
         T.Assert (not U1.Is_Null, "Null user returned by Verify_Session");
         T.Assert (not S1.Get_Start_Date.Is_Null, "Session must be started");
         T.Assert (S1.Get_End_Date.Is_Null, "Session must not be finished");
         Util.Tests.Assert_Equals (T, Principal.Session.Get_Start_Date.Value,
                                   S1.Get_Start_Date.Value,
                                   "Invalid start date");

         Principal.Manager.Close_Session (Principal.Session.Get_Id);
      end;

   end Test_Create_User;

   --  ------------------------------
   --  Test logout of a user
   --  ------------------------------
   procedure Test_Logout_User (T : in out Test) is
      Principal : Helpers.Test_User;
   begin
      --  Create the user
      Helpers.Create_User (Principal);
      T.Assert (not Principal.User.Is_Null, "User is created");
      T.Assert (Principal.User.Get_Id > 0, "User has an allocated key");
      T.Assert (Principal.Email.Get_Id > 0, "Email has an allocated key");

      Helpers.Logout (Principal);

      --  Verify the user session
      declare
         S1 : Session_Ref;
         U1 : User_Ref;
      begin
         Principal.Manager.Verify_Session (Id => Principal.Session.Get_Id,
                                           Session => S1, User => U1);

         T.Assert (False, "Verify_Session should report a non-existent session");

      exception
         when Not_Found =>
            null;
      end;

      begin
         Helpers.Logout (Principal);

         T.Assert (False, "Second logout should report a non-existent session");

      exception
         when Not_Found =>
            null;
      end;
   end Test_Logout_User;

   --  ------------------------------
   --  Test creation of a user
   --  ------------------------------
   procedure Test_Login_User (T : in out Test) is
      use type Ada.Calendar.Time;

      Principal : Helpers.Test_User;
      T1        : Ada.Calendar.Time;
   begin
      begin
         Principal.Email.Set_Email ("nobody@gmail.com");
         Helpers.Login (Principal);
         T.Assert (False, "Login succeeded with an invalid user name");

         exception
         when Not_Found =>
            null;
      end;

      --  Create the user
      T1 := Ada.Calendar.Clock;
      Helpers.Create_User (Principal);
      Helpers.Logout (Principal);

      Helpers.Login (Principal);

      T.Assert (not Principal.User.Is_Null, "User is created");
      T.Assert (Principal.User.Get_Id > 0, "User has an allocated key");
      T.Assert (Principal.Email.Get_Id > 0, "Email has an allocated key");
      T.Assert (not Principal.Session.Is_Null, "Session is not created");

      --  Verify the user session
      declare
         S1 : Session_Ref;
         U1 : User_Ref;
         T2 : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         Principal.Manager.Verify_Session (Id => Principal.Session.Get_Id,
                                           Session => S1, User => U1);

         T.Assert (not S1.Is_Null, "Null session returned by Verify_Session");
         T.Assert (not U1.Is_Null, "Null user returned by Verify_Session");
         T.Assert (not S1.Get_Start_Date.Is_Null, "Session start date must not be null");
         T.Assert (S1.Get_End_Date.Is_Null, "Session end date must be null");
         Util.Tests.Assert_Equals (T, Principal.Session.Get_Start_Date.Value,
                                   S1.Get_Start_Date.Value,
                                   "Invalid start date");

         --  Storing a date in the database will loose some precision.
         T.Assert (S1.Get_Start_Date.Value >= T1 - 1.0, "Start date is invalid 1");
         T.Assert (S1.Get_Start_Date.Value <= T2 + 10.0, "Start date is invalid 3");

         Principal.Manager.Close_Session (Principal.Session.Get_Id);
      end;

   end Test_Login_User;

   --  ------------------------------
   --  Test password reset process
   --  ------------------------------
   procedure Test_Reset_Password_User (T : in out Test) is
      Principal : Helpers.Test_User;
      Key     : AWA.Users.Models.Access_Key_Ref;
   begin
      --  Create the user
      Helpers.Create_User (Principal);
      Helpers.Logout (Principal);

      --  Start the lost password process.
      Principal.Manager.Lost_Password (Email => Principal.Email.Get_Email);
      Helpers.Login (Principal);

      --  Get the access key to reset the password
      declare
         DB    : ADO.Sessions.Session := Principal.Manager.Get_Session;
         Query : ADO.SQL.Query;
         Found : Boolean;
      begin
         --  Find the access key
         Query.Set_Filter ("user_id = ?");
         Query.Bind_Param (1, Principal.User.Get_Id);
         Key.Find (DB, Query, Found);
         T.Assert (Found, "Access key for lost_password process not found");

         Principal.Manager.Reset_Password (Key      => Key.Get_Access_Key,
                                           Password => "newadmin",
                                           IpAddr   => "192.168.1.2",
                                           User     => Principal.User,
                                           Session  => Principal.Session);

         --  Search the access key again, it must have been removed.
         Key.Find (DB, Query, Found);
         T.Assert (not Found, "The access key is still present in the database");
      end;

      T.Assert (not Principal.User.Is_Null, "User is created");
      T.Assert (Principal.User.Get_Id > 0, "User has an allocated key");
      T.Assert (Principal.Email.Get_Id > 0, "Email has an allocated key");
      T.Assert (not Principal.Session.Is_Null, "Session is not created");

      Helpers.Logout (Principal);

   end Test_Reset_Password_User;

   --  ------------------------------
   --  Test Get_User_Module operation
   --  ------------------------------
   procedure Test_Get_Module (T : in out Test) is
      use type AWA.Users.Module.User_Module_Access;
   begin
      declare
         M : constant AWA.Users.Module.User_Module_Access := AWA.Users.Module.Get_User_Module;
      begin
         T.Assert (M /= null, "Get_User_Module returned null");
      end;
      declare
         S : Util.Measures.Stamp;
         M : AWA.Users.Module.User_Module_Access;
      begin
         for I in 1 .. 1_000 loop
            M := AWA.Users.Module.Get_User_Module;
         end loop;
         Util.Measures.Report (S, "Get_User_Module (1000)");
         T.Assert (M /= null, "Get_User_Module returned null");
      end;
   end Test_Get_Module;

end AWA.Users.Services.Tests;
