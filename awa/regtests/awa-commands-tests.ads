-----------------------------------------------------------------------
--  awa-commands-tests -- Test the AWA.Commands
--  Copyright (C) 2020 Stephane Carrez
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
with Ada.Strings.Unbounded;

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

   --  Test the command with various logging options.
   procedure Test_Verbose_Command (T : in out Test);

   procedure Execute (T       : in out Test;
                      Command : in String;
                      Input   : in String;
                      Output  : in String;
                      Result  : out Ada.Strings.Unbounded.Unbounded_String;
                      Status  : in Natural := 0);

end AWA.Commands.Tests;
