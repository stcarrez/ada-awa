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

with AWA.Users.Model;
with ADO.Sessions;
with ADO.SQL;
package body AWA.Users.Logic.Tests.Helpers is

   --  ------------------------------
   --  Create a test user for a new test and get an open session.
   --  ------------------------------
   procedure Create_User (Principal : in out Test_User) is
      Key     : AWA.Users.Model.Access_Key_Ref;
   begin
      if Principal.Manager = null then
         Principal.Manager := AWA.Users.Logic.Create_User_Manager (AWA.Users.Module.Instance);
      end if;
      Principal.User.Set_First_Name ("Joe");
      Principal.User.Set_Last_Name ("Pot");
      Principal.User.Set_Password ("admin");
      Principal.Email.Set_Email ("Joe-" & Util.Tests.Get_UUID & "@gmail.com");
      Principal.Manager.Create_User (Principal.User, Principal.Email);

      declare
         DB    : ADO.Sessions.Session := Principal.Manager.Get_Session;
         Query : ADO.SQL.Query;
         Found : Boolean;
      begin
         --  Find the access key
         Query.Set_Filter ("user_id = ?");
         Query.Bind_Param (1, Principal.User.Get_Id);
         Key.Find (DB, Query, Found);
      end;

      --  Run the verification and get the user and its session
      Principal.Manager.Verify_User (Key.Get_Access_Key, "192.168.1.1",
                                     Principal.User, Principal.Session);
   end Create_User;

   --  ------------------------------
   --  Login a user and create a session
   --  ------------------------------
   procedure Login (Principal : in out Test_User) is
   begin
      if Principal.Manager = null then
         Principal.Manager := AWA.Users.Logic.Create_User_Manager (AWA.Users.Module.Instance);
      end if;

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
      if Principal.Manager = null then
         Principal.Manager := AWA.Users.Logic.Create_User_Manager (AWA.Users.Module.Instance);
      end if;
      Principal.Manager.Close_Session (Principal.Session.Get_Id);
   end Logout;

end AWA.Users.Logic.Tests.Helpers;
