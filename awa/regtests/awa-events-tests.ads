-----------------------------------------------------------------------
--  events-tests -- Unit tests for AWA events
--  Copyright (C) 2012 Stephane Carrez
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
with AWA.Tests;

package AWA.Events.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   --  Test searching an event name in the definition list.
   procedure Test_Find_Event (T : in out Test);

   --  Test creation and initialization of event manager.
   procedure Test_Initialize (T : in out Test);

   --  Test adding an action.
   procedure Test_Add_Action (T : in out Test);

   --  Test dispatching events
   procedure Test_Dispatch (T : in out Test);

end AWA.Events.Tests;
