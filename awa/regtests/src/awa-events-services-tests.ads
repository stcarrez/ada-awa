-----------------------------------------------------------------------
--  events-tests -- Unit tests for AWA events
--  Copyright (C) 2012, 2019, 2022 Stephane Carrez
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
private with Util.Beans.Methods;

package AWA.Events.Services.Tests is

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

   --  Test dispatching event through a database queue.
   procedure Test_Dispatch_Persist (T : in out Test);

   --  Test dispatching synchronous event to a dynamic bean (created on demand).
   procedure Test_Dispatch_Synchronous_Dyn (T : in out Test);

   --  Test dispatching synchronous event to a dynamic bean and raise an exception in the action.
   procedure Test_Dispatch_Synchronous_Raise (T : in out Test);

   procedure Dispatch_Event (T            : in out Test;
                             Kind         : in Event_Index;
                             Expect_Count : in Natural;
                             Expect_Prio  : in Natural);

private

   type Action_Bean is new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Count           : Natural := 0;
      Priority        : Integer := 0;
      Raise_Exception : Boolean := False;
   end record;
   type Action_Bean_Access is access all Action_Bean'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Action_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Action_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   overriding
   function Get_Method_Bindings (From : in Action_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   procedure Event_Action (From  : in out Action_Bean;
                           Event : in AWA.Events.Module_Event'Class);

   function Create_Action_Bean return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Events.Services.Tests;
