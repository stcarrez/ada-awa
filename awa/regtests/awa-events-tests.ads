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

   --  Test the Get_Event_Type_Name internal operation.
   procedure Test_Get_Event_Name (T : in out Test);

   --  Test creation and initialization of event manager.
   procedure Test_Initialize (T : in out Test);

   --  Test adding an action.
   procedure Test_Add_Action (T : in out Test);

   --  Test dispatching synchronous event to a global bean.
   procedure Test_Dispatch_Synchronous (T : in out Test);

   --  Test dispatching event through a fifo queue.
   procedure Test_Dispatch_Fifo (T : in out Test);

   --  Test dispatching synchronous event to a dynamic bean (created on demand).
   procedure Test_Dispatch_Synchronous_Dyn (T : in out Test);

   --  Test dispatching synchronous event to a dynamic bean and raise an exception in the action.
   procedure Test_Dispatch_Synchronous_Raise (T : in out Test);

   procedure Dispatch_Event (T            : in out Test;
                             Kind         : in Event_Index;
                             Expect_Count : in Natural;
                             Expect_Prio  : in Natural);

end AWA.Events.Tests;
