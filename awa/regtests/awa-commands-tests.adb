-----------------------------------------------------------------------
--  awa-commands-tests -- Test the AWA.Commands
--  Copyright (C) 2020, 2022 Stephane Carrez
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

with Ada.Text_IO;
with Ada.Exceptions;
with Util.Processes;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Test_Caller;
with Util.Log.Loggers;
with Security;
with ASF.Tests;
with AWA.Tests.Helpers.Users;
with AWA.Users.Services;
with AWA.Users.Models;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;

package body AWA.Commands.Tests is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Commands.Tests");

   package Caller is new Util.Test_Caller (Test, "Commands");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Commands.Start_Stop",
                       Test_Start_Stop'Access);
      Caller.Add_Test (Suite, "Test AWA.Commands.List (tables)",
                       Test_List_Tables'Access);
      Caller.Add_Test (Suite, "Test AWA.Commands.List (users)",
                       Test_List_Users'Access);
      Caller.Add_Test (Suite, "Test AWA.Commands.List (sessions)",
                       Test_List_Sessions'Access);
      Caller.Add_Test (Suite, "Test AWA.Commands.List (jobs)",
                       Test_List_Jobs'Access);
      Caller.Add_Test (Suite, "Test AWA.Commands.Info (secure configuration 1)",
                       Test_Secure_Configuration'Access);
      Caller.Add_Test (Suite, "Test AWA.Commands.Info (secure configuration 2)",
                       Test_Secure_Configuration_2'Access);
      Caller.Add_Test (Suite, "Test AWA.Commands.Info (verbose)",
                       Test_Verbose_Command'Access);
      Caller.Add_Test (Suite, "Test AWA.Commands.User (register)",
                       Test_User_Command'Access);
   end Add_Tests;

   --  ------------------------------
   --  Execute the command and get the output in a string.
   --  ------------------------------
   procedure Execute (T       : in out Test;
                      Command : in String;
                      Input   : in String;
                      Output  : in String;
                      Result  : out Ada.Strings.Unbounded.Unbounded_String;
                      Status  : in Natural := 0) is
      P        : aliased Util.Streams.Pipes.Pipe_Stream;
      Buffer   : Util.Streams.Buffered.Input_Buffer_Stream;
   begin
      if Input'Length > 0 then
         Log.Info ("Execute: {0} < {1}", Command, Input);
      elsif Output'Length > 0 then
         Log.Info ("Execute: {0} > {1}", Command, Output);
      else
         Log.Info ("Execute: {0}", Command);
      end if;
      P.Set_Input_Stream (Input);
      P.Set_Output_Stream (Output);
      P.Open (Command, Util.Processes.READ_ALL);

      --  Write on the process input stream.
      Result := Ada.Strings.Unbounded.Null_Unbounded_String;
      Buffer.Initialize (P'Unchecked_Access, 8192);
      Buffer.Read (Result);
      P.Close;
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Result));
      Log.Info ("Command result: {0}", Result);
      Util.Tests.Assert_Equals (T, Status, P.Get_Exit_Status, "Command '" & Command & "' failed");
   end Execute;

   --  ------------------------------
   --  Test start and stop command.
   --  ------------------------------
   procedure Test_Start_Stop (T : in out Test) is

      Config : constant String := Util.Tests.Get_Parameter ("test_config_path");

      task Start_Server is
         entry Start;
         entry Wait (Result : out Ada.Strings.Unbounded.Unbounded_String);
      end Start_Server;

      task body Start_Server is
         Output : Ada.Strings.Unbounded.Unbounded_String;
      begin
         accept Start do
            null;
         end Start;
         begin
            T.Execute ("bin/awa_command -c " & Config & " start -m 26123",
                       "", "", Output, 0);
         exception
            when others =>
               Output := Ada.Strings.Unbounded.To_Unbounded_String ("* exception *");
         end;
         accept Wait (Result : out Ada.Strings.Unbounded.Unbounded_String) do
            Result := Output;
         end Wait;
      end Start_Server;

      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Launch the 'start' command in a separate task because the command will hang.
      Start_Server.Start;
      delay 5.0;

      --  Launch the 'stop' command, this should terminate the 'start' command.
      T.Execute ("bin/awa_command -c " & Config & " stop -m 26123",
                 "", "", Result, 0);

      --  Wait for the task result.
      Start_Server.Wait (Result);
      Util.Tests.Assert_Matches (T, "Starting...", Result, "AWA start command output");
   end Test_Start_Stop;

   procedure Test_List_Tables (T : in out Test) is
      Config : constant String := Util.Tests.Get_Parameter ("test_config_path");
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("bin/awa_command -c " & Config & " list -t",
                 "", "", Result, 0);
      Util.Tests.Assert_Matches (T, "awa_audit *[0-9]+", Result, "Missing awa_audit");
      Util.Tests.Assert_Matches (T, "awa_session *[0-9]+", Result, "Missing awa_session");
      Util.Tests.Assert_Matches (T, "entity_type *[0-9]+", Result, "Missing entity_type");
      Util.Tests.Assert_Matches (T, "awa_wiki_page *[0-9]+", Result, "Missing awa_wiki_page");
   end Test_List_Tables;

   --  ------------------------------
   --  Test the list -u command.
   --  ------------------------------
   procedure Test_List_Users (T : in out Test) is
      Config : constant String := Util.Tests.Get_Parameter ("test_config_path");
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("bin/awa_command -c " & Config & " list -u",
                 "", "", Result, 0);
      Util.Tests.Assert_Matches (T, "Joe Pot", Result, "Missing user");
      Util.Tests.Assert_Matches (T, "test-wiki@test.com", Result, "Missing email");
   end Test_List_Users;

   --  ------------------------------
   --  Test the list -s command.
   --  ------------------------------
   procedure Test_List_Sessions (T : in out Test) is
      Config : constant String := Util.Tests.Get_Parameter ("test_config_path");
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("bin/awa_command -c " & Config & " list -s",
                 "", "", Result, 0);
      Util.Tests.Assert_Matches (T, "Joe Pot", Result, "Missing user");
   end Test_List_Sessions;

   --  ------------------------------
   --  Test the list -j command.
   --  ------------------------------
   procedure Test_List_Jobs (T : in out Test) is
      Config : constant String := Util.Tests.Get_Parameter ("test_config_path");
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("bin/awa_command -c " & Config & " list -j",
                 "", "", Result, 0);
      Util.Tests.Assert_Matches (T, "S_FACTORY", Result, "Missing factory");
      Util.Tests.Assert_Matches (T, "Joe Pot", Result, "Missing user");
   end Test_List_Jobs;

   --  ------------------------------
   --  Test the command with a secure keystore configuration.
   --  ------------------------------
   procedure Test_Secure_Configuration (T : in out Test) is
      Config   : constant String := Util.Tests.Get_Parameter ("test_config_path");
      Keystore : constant String := Util.Tests.Get_Parameter ("test_keystore_file");
      Result   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("bin/awa_command -c " & Config & " info --keystore " & Keystore
                   & " --password=unit-test-password", "", "", Result, 0);
      Util.Tests.Assert_Matches (T, "app_name *AWA Secure Demo", Result,
                                 "Secure property not accessed");
   end Test_Secure_Configuration;

   --  ------------------------------
   --  Test the command with a secure keystore configuration.
   --  ------------------------------
   procedure Test_Secure_Configuration_2 (T : in out Test) is
      Config   : constant String := Util.Tests.Get_Parameter ("test_secure_config_path");
      Result   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("bin/awa_command -c " & Config & " info", "", "", Result, 0);
      Util.Tests.Assert_Matches (T, "app_name *AWA Secure Demo", Result,
                                 "Secure property not accessed");
      Util.Tests.Assert_Matches (T, "users.auth_key *auth-", Result,
                                 "Secure property not accessed");
      Util.Tests.Assert_Matches (T, "users.server_id *[123]0", Result,
                                 "Secure property not accessed");
   end Test_Secure_Configuration_2;

   --  ------------------------------
   --  Test the command with various logging options.
   --  ------------------------------
   procedure Test_Verbose_Command (T : in out Test) is
      Config   : constant String := Util.Tests.Get_Parameter ("test_config_path");
      Result   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Execute ("bin/awa_command -v -c " & Config & " info ", "", "", Result, 0);
      Util.Tests.Assert_Matches (T, "INFO : Using application search dir:", Result,
                                 "Missing log message");
   end Test_Verbose_Command;

   --  ------------------------------
   --  Test the user command.
   --  ------------------------------
   procedure Test_User_Command (T : in out Test) is
      use type Security.Principal_Access;

      Email     : constant String := "Reg-" & Util.Tests.Get_Uuid & "@register.com";
      Config    : constant String := Util.Tests.Get_Parameter ("test_config_path");
      Result    : Ada.Strings.Unbounded.Unbounded_String;
      Principal : AWA.Tests.Helpers.Users.Test_User;
      Key       : AWA.Users.Models.Access_Key_Ref;
   begin
      T.Execute ("bin/awa_command -c " & Config & " user " & Email & " --register",
                 "", "", Result, 0);
      Util.Tests.Assert_Matches (T, "User 'Reg-.*@register.com' is now registered", Result,
                                 "Missing notice message");
      Util.Tests.Assert_Matches (T, "Registration key: .*", Result,
                                 "Missing registration key in message");

      begin
         Principal.Email.Set_Email (Email);
         AWA.Tests.Helpers.Users.Login (Principal);
         T.Assert (False, "Login succeeded with a registered user");

      exception
         when E : AWA.Users.Services.User_Disabled =>
            Util.Tests.Assert_Matches (T, "User account is disabled",
                                       Ada.Exceptions.Exception_Message (E));
      end;
      AWA.Tests.Helpers.Users.Find_Access_Key (Principal, Email, Key);

      --  Run the verification but we should not get a session.
      Principal.Manager.Verify_User (Key.Get_Access_Key, "192.168.1.1",
                                     Principal.Principal);

      declare
         Request    : ASF.Requests.Mockup.Request;
         Reply      : ASF.Responses.Mockup.Response;
         Access_Key : constant String := Key.Get_Access_Key;
      begin
         ASF.Tests.Do_Get (Request, Reply, "/auth/validate/" & Access_Key,
                           "validate-user.html");

         Util.Tests.Assert_Equals (T, ASF.Responses.Sc_Moved_Temporarily, Reply.Get_Status,
                                      "Invalid response");
         ASF.Tests.Assert_Redirect (T, "/asfunit/auth/change-password/" & Access_Key,
                                    Reply, "Invalid redirect after verify");

         --  Simulate a user clicking on the reset password form.
         --  This verifies the key, login the user and redirect him to the change-password page
         Request.Set_Parameter ("key", Access_Key);
         Request.Set_Parameter ("password", "admin");
         Request.Set_Parameter ("reset-password", "1");
         ASF.Tests.Do_Post (Request, Reply, "/auth/change-password/" & Access_Key,
                            "reset-password-3.html");

         Util.Tests.Assert_Equals (T, ASF.Responses.Sc_Moved_Temporarily, Reply.Get_Status,
                                      "Invalid response");
         ASF.Tests.Assert_Redirect (T, "/asfunit/workspaces/main.html",
                                    Reply, "Invalid redirect after change-password");

         --  Check that the user is logged and we have a user principal now.
         T.Assert (Request.Get_User_Principal /= null,
                   "A user principal should be defined");

      end;
   end Test_User_Command;

end AWA.Commands.Tests;
