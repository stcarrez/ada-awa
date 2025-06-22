-----------------------------------------------------------------------
--  awa-users-services-tests -- Unit tests for user service
--  Copyright (C) 2009, 2010, 2011, 2012, 2017, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Measures;
with Util.Log.Loggers;

with ADO;
with ADO.Sessions;
with ADO.SQL;
with ADO.Objects;
with Ada.Calendar;

with AWA.Users.Modules;
with AWA.Tests.Helpers.Users;
package body AWA.Users.Services.Tests is

   use ADO;
   use ADO.Objects;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Users.Services.Tests");

   package Caller is new Util.Test_Caller (Test, "Users.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
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
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Update_User",
                       Test_Disable_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Users.Services.Create_User",
                       Test_Create_User_No_Verify'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of a user
   --  ------------------------------
   procedure Test_Create_User (T : in out Test) is
      Principal : AWA.Tests.Helpers.Users.Test_User;
   begin
      --  Create the user
      AWA.Tests.Helpers.Users.Create_User (Principal);
      T.Assert (not Principal.User.Is_Null, "User is created");
      T.Assert (Principal.User.Get_Id > 0, "User has an allocated key");
      T.Assert (Principal.Email.Get_Id > 0, "Email has an allocated key");
      T.Assert (not Principal.Session.Is_Null, "Session must be created");
      T.Assert (Principal.Session.Get_End_Date.Is_Null, "Session must be opened");

      --  Verify the user session
      declare
         use type Ada.Calendar.Time;
         S1 : Session_Ref;
         U1 : User_Ref;
         T1 : constant Ada.Calendar.Time := Principal.Session.Get_Start_Date;
      begin
         Principal.Manager.Verify_Session (Id => Principal.Session.Get_Id,
                                           Session => S1, User => U1);

         T.Assert (not S1.Is_Null, "Null session returned by Verify_Session");
         T.Assert (not U1.Is_Null, "Null user returned by Verify_Session");
         T.Assert (S1.Get_End_Date.Is_Null, "Session must not be finished");

         --  After we read the date/time from the database, we can loose
         --  the sub-second precision and we can't safely compare with the
         --  original.
         T.Assert (T1 - 1.0 <= S1.Get_Start_Date, "Invalid start date");
         T.Assert (T1 + 1.0 >= S1.Get_Start_Date, "Invalid start date");

         Principal.Manager.Close_Session (Principal.Session.Get_Id);
      end;

   end Test_Create_User;

   --  ------------------------------
   --  Test logout of a user
   --  ------------------------------
   procedure Test_Logout_User (T : in out Test) is
      Principal : AWA.Tests.Helpers.Users.Test_User;
   begin
      --  Create the user
      AWA.Tests.Helpers.Users.Create_User (Principal);
      T.Assert (not Principal.User.Is_Null, "User is created");
      T.Assert (Principal.User.Get_Id > 0, "User has an allocated key");
      T.Assert (Principal.Email.Get_Id > 0, "Email has an allocated key");

      AWA.Tests.Helpers.Users.Logout (Principal);

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
         AWA.Tests.Helpers.Users.Logout (Principal);

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

      Principal : AWA.Tests.Helpers.Users.Test_User;
      T1        : Ada.Calendar.Time;
   begin
      begin
         Principal.Email.Set_Email ("nobody@gmail.com");
         AWA.Tests.Helpers.Users.Login (Principal);
         T.Assert (False, "Login succeeded with an invalid user name");

      exception
         when Not_Found =>
            null;
      end;

      --  Create the user
      T1 := Ada.Calendar.Clock;
      AWA.Tests.Helpers.Users.Create_User (Principal);
      Log.Info ("Created user and session is {0}",
                ADO.Identifier'Image (Principal.Session.Get_Id));
      AWA.Tests.Helpers.Users.Logout (Principal);

      delay 1.0;
      AWA.Tests.Helpers.Users.Login (Principal);
      Log.Info ("Logout and login with session {0}",
                ADO.Identifier'Image (Principal.Session.Get_Id));

      T.Assert (not Principal.User.Is_Null, "User is created");
      T.Assert (Principal.User.Get_Id > 0, "User has an allocated key");
      T.Assert (Principal.Email.Get_Id > 0, "Email has an allocated key");
      T.Assert (not Principal.Session.Is_Null, "Session is not created");

      --  Verify the user session
      declare
         S1 : Session_Ref;
         U1 : User_Ref;
         Ts : constant Ada.Calendar.Time := Principal.Session.Get_Start_Date;
         T2 : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         Principal.Manager.Verify_Session (Id => Principal.Session.Get_Id,
                                           Session => S1, User => U1);

         T.Assert (not S1.Is_Null, "Null session returned by Verify_Session");
         T.Assert (not U1.Is_Null, "Null user returned by Verify_Session");
         T.Assert (S1.Get_End_Date.Is_Null, "Session end date must be null");
         T.Assert (Ts - 1.0 <= S1.Get_Start_Date, "Invalid start date 1");
         T.Assert (Ts + 1.0 >= S1.Get_Start_Date, "Invalid start date 2");

         --  Storing a date in the database will loose some precision.
         T.Assert (S1.Get_Start_Date >= T1 - 1.0, "Start date is invalid 1");
         T.Assert (S1.Get_Start_Date <= T2 + 10.0, "Start date is invalid 3");

         Principal.Manager.Close_Session (Principal.Session.Get_Id);
      end;

   end Test_Login_User;

   --  ------------------------------
   --  Test password reset process
   --  ------------------------------
   procedure Test_Reset_Password_User (T : in out Test) is
      use type AWA.Users.Principals.Principal_Access;

      Principal : AWA.Tests.Helpers.Users.Test_User;
      Key       : AWA.Users.Models.Access_Key_Ref;
   begin
      --  Create the user
      AWA.Tests.Helpers.Users.Create_User (Principal);
      AWA.Tests.Helpers.Users.Logout (Principal);

      --  Start the lost password process.
      Principal.Manager.Lost_Password (Email => Principal.Email.Get_Email);
      AWA.Tests.Helpers.Users.Login (Principal);

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

         Principal.Manager.Reset_Password (Key       => Key.Get_Access_Key,
                                           Password  => "newadmin",
                                           IpAddr    => "192.168.1.2",
                                           Principal => Principal.Principal);
         T.Assert (Principal.Principal /= null, "No principal returned");

         Principal.User    := Principal.Principal.Get_User;
         Principal.Session := Principal.Principal.Get_Session;

         --  Search the access key again, it must have been removed.
         Key.Find (DB, Query, Found);
         T.Assert (not Found, "The access key is still present in the database");
      end;

      T.Assert (not Principal.User.Is_Null, "User is created");
      T.Assert (Principal.User.Get_Id > 0, "User has an allocated key");
      T.Assert (Principal.Email.Get_Id > 0, "Email has an allocated key");
      T.Assert (not Principal.Session.Is_Null, "Session is not created");

      AWA.Tests.Helpers.Users.Logout (Principal);

   end Test_Reset_Password_User;

   --  ------------------------------
   --  Test Get_User_Module operation
   --  ------------------------------
   procedure Test_Get_Module (T : in out Test) is
      use type AWA.Users.Modules.User_Module_Access;
   begin
      declare
         M : constant AWA.Users.Modules.User_Module_Access := AWA.Users.Modules.Get_User_Module;
      begin
         T.Assert (M /= null, "Get_User_Module returned null");
      end;
      declare
         S : Util.Measures.Stamp;
         M : AWA.Users.Modules.User_Module_Access;
      begin
         for I in 1 .. 1_000 loop
            M := AWA.Users.Modules.Get_User_Module;
         end loop;
         Util.Measures.Report (S, "Get_User_Module (1000)");
         T.Assert (M /= null, "Get_User_Module returned null");
      end;
   end Test_Get_Module;

   --  ------------------------------
   --  Disable a user and check login is refused.
   --  ------------------------------
   procedure Test_Disable_User (T : in out Test) is
      Principal : AWA.Tests.Helpers.Users.Test_User;
   begin
      --  Create the user
      AWA.Tests.Helpers.Users.Create_User (Principal);
      AWA.Tests.Helpers.Users.Logout (Principal);

      Principal.Manager.Update_User (Email => Principal.Email.Get_Email,
                                     Status => Models.USER_DISABLED);

      begin
         AWA.Tests.Helpers.Users.Login (Principal);
         T.Assert (False, "Login succeeded with a disabled user");

      exception
         when User_Disabled =>
            null;
      end;

      Principal.Manager.Update_User (Email => Principal.Email.Get_Email,
                                     Status => Models.USER_ENABLED);

      AWA.Tests.Helpers.Users.Login (Principal);
      T.Assert (not Principal.User.Is_Null, "User is created");
      T.Assert (not Principal.Session.Is_Null, "Session is not created");

   end Test_Disable_User;

   --  ------------------------------
   --  Create a user and try to login without the verify.
   --  ------------------------------
   procedure Test_Create_User_No_Verify (T : in out Test) is
      Principal : AWA.Tests.Helpers.Users.Test_User;
      Key       : AWA.Users.Models.Access_Key_Ref;
      Email     : constant String := "Joe-" & Util.Tests.Get_Uuid & "@gmail.com";
   begin
      AWA.Tests.Helpers.Users.Initialize (Principal);
      Principal.User.Set_First_Name ("Joe");
      Principal.User.Set_Last_Name ("Pot");
      Principal.Email.Set_Email (Email);
      Principal.Manager.Create_User (Principal.User, Principal.Email, "admin", Key, False);

      begin
         AWA.Tests.Helpers.Users.Login (Principal);
         T.Fail ("No User_Disabled exception was raised");

      exception
         when User_Disabled =>
            null;
      end;
   end Test_Create_User_No_Verify;

end AWA.Users.Services.Tests;
