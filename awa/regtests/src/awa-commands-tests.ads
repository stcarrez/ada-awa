-----------------------------------------------------------------------
--  awa-commands-tests -- Test the AWA.Commands
--  Copyright (C) 2020, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package AWA.Commands.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test start and stop command.
   procedure Test_Start_Stop (T : in out Test);

   procedure Test_List_Tables (T : in out Test);

   --  Test the list -u command.
   procedure Test_List_Users (T : in out Test);

   --  Test the list -s command.
   procedure Test_List_Sessions (T : in out Test);

   --  Test the list -j command.
   procedure Test_List_Jobs (T : in out Test);

   --  Test the command with a secure keystore configuration.
   procedure Test_Secure_Configuration (T : in out Test);

   procedure Test_Secure_Configuration_2 (T : in out Test);

   --  Test the command with various logging options.
   procedure Test_Verbose_Command (T : in out Test);

   --  Test the user command.
   procedure Test_User_Command (T : in out Test);

end AWA.Commands.Tests;
