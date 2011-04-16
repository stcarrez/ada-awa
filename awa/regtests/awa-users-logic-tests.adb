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

with Util.Tests;
with Util.Test_Caller;

with ADO;
with ADO.Sessions;
with ADO.SQL;
with ADO.Objects;
with Ada.Calendar;

with AWA.Users.logic.Tests.Helpers;
package body AWA.Users.Logic.Tests is

   use Util.Tests;
   use ADO;
   use ADO.Objects;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Users.Logic.Create_User",
                       Test_Create_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Logic.Close_Session",
                       Test_Logout_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Logic.Authenticate, Close_Session",
                       Test_Login_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Logic.Lost_Password, Reset_Password",
                       Test_Reset_Password_User'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of a user
   --  ------------------------------
   procedure Test_Create_User (T : in out Test) is
      Principal : Helpers.Test_User;
   begin
      --  Create the user
      Helpers.Create_User (Principal);
      Assert (T, not Principal.User.Is_Null, "User is created");
      Assert (T, Principal.User.Get_Id > 0, "User has an allocated key");
      Assert (T, Principal.Email.Get_Id > 0, "Email has an allocated key");
      Assert (T, not Principal.Session.Is_Null, "Session must be created");
      Assert (T, Principal.Session.Get_End_Date.Is_Null, "Session must be opened");

      --  Verify the user session
      declare
         S1 : Session_Ref;
         U1 : User_Ref;
      begin
         Principal.Manager.Verify_Session (Id => Principal.Session.Get_Id,
                                           Session => S1, User => U1);

         Assert (T, not S1.Is_Null, "Null session returned by Verify_Session");
         Assert (T, not U1.Is_Null, "Null user returned by Verify_Session");
         Assert (T, not S1.Get_Start_Date.Is_Null, "Session must be started");
         Assert (T, S1.Get_End_Date.Is_Null, "Session must not be finished");
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
      Assert (T, not Principal.User.Is_Null, "User is created");
      Assert (T, Principal.User.Get_Id > 0, "User has an allocated key");
      Assert (T, Principal.Email.Get_Id > 0, "Email has an allocated key");

      Helpers.Logout (Principal);

      --  Verify the user session
      declare
         S1 : Session_Ref;
         U1 : User_Ref;
      begin
         Principal.Manager.Verify_Session (Id => Principal.Session.Get_Id,
                                           Session => S1, User => U1);

         Assert (T, False, "Verify_Session should report a non-existent session");

      exception
         when Not_Found =>
            null;
      end;

      begin
         Helpers.Logout (Principal);

         Assert (T, False, "Second logout should report a non-existent session");

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
      T1        : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      begin
         Principal.Email.Set_Email ("nobody@gmail.com");
         Helpers.Login (Principal);
         Assert (T, False, "Login succeeded with an invalid user name");

         exception
         when Not_Found =>
            null;
      end;

      --  Create the user
      Helpers.Create_User (Principal);
      Helpers.Logout (Principal);

      Helpers.Login (Principal);

      Assert (T, not Principal.User.Is_Null, "User is created");
      Assert (T, Principal.User.Get_Id > 0, "User has an allocated key");
      Assert (T, Principal.Email.Get_Id > 0, "Email has an allocated key");
      Assert (T, not Principal.Session.Is_Null, "Session is not created");

      --  Verify the user session
      declare
         S1 : Session_Ref;
         U1 : User_Ref;
         T2 : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         Principal.Manager.Verify_Session (Id => Principal.Session.Get_Id,
                                           Session => S1, User => U1);

         Assert (T, not S1.Is_Null, "Null session returned by Verify_Session");
         Assert (T, not U1.Is_Null, "Null user returned by Verify_Session");
         Assert (T, not S1.Get_Start_Date.Is_Null, "Session start date must not be null");
         Assert (T, S1.Get_End_Date.Is_Null, "Session end date must be null");
         Util.Tests.Assert_Equals (T, Principal.Session.Get_Start_Date.Value,
                                   S1.Get_Start_Date.Value,
                                   "Invalid start date");
         Util.Tests.Assert_Equals (T, T1,
                                   S1.Get_Start_Date.Value,
                                   "Invalid start date 3");

         Assert (T, T2 >= S1.Get_Start_Date.Value, "Start date is invalid 1");
         Assert (T, T1 <= S1.Get_Start_Date.Value + 1.0, "Start date is invalid 2");

         Principal.Manager.Close_Session (Principal.Session.Get_Id);
      end;

   end Test_Login_User;

   --  ------------------------------
   --  Test password reset process
   --  ------------------------------
   procedure Test_Reset_Password_User (T : in out Test) is
      Principal : Helpers.Test_User;
      Key     : AWA.Users.Model.Access_Key_Ref;
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
         Assert (T, Found, "Access key for lost_password process not found");

         Principal.Manager.Reset_Password (Key      => Key.Get_Access_Key,
                                           Password => "newadmin",
                                           IpAddr   => "192.168.1.2",
                                           User     => Principal.User,
                                           Session  => Principal.Session);

         --  Search the access key again, it must have been removed.
         Key.Find (DB, Query, Found);
         Assert (T, not Found, "The access key is still present in the database");
      end;

      Assert (T, not Principal.User.Is_Null, "User is created");
      Assert (T, Principal.User.Get_Id > 0, "User has an allocated key");
      Assert (T, Principal.Email.Get_Id > 0, "Email has an allocated key");
      Assert (T, not Principal.Session.Is_Null, "Session is not created");

      Helpers.Logout (Principal);

   end Test_Reset_Password_User;

end AWA.Users.Logic.Tests;
