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
with Util.Test_Caller;
with Util.Beans.Methods;
with Util.Log.Loggers;
with Util.Measures;

with EL.Beans;
with EL.Expressions;
with EL.Contexts.Default;

with ASF.Applications;
with AWA.Applications;
with AWA.Applications.Configs;
with AWA.Applications.Factory;
with AWA.Events.Action_Method;
with AWA.Tests;
with ADO.Sessions;

with AWA.Events.Queues;
with AWA.Events.Services;

package body AWA.Events.Tests is

   use AWA.Events.Services;

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Events.Tests");

   package Event_Test_4 is new AWA.Events.Definition (Name => "event-test-4");
   package Event_Test_1 is new AWA.Events.Definition (Name => "event-test-1");
   package Event_Test_3 is new AWA.Events.Definition (Name => "event-test-3");
   package Event_Test_2 is new AWA.Events.Definition (Name => "event-test-2");
   package Event_Test_5 is new AWA.Events.Definition (Name => "event-test-5");

   package Caller is new Util.Test_Caller (Test, "Events.Tests");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Events.Find_Event_Index",
                       Test_Find_Event'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Initialize",
                       Test_Initialize'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Add_Action",
                       Test_Add_Action'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Dispatch",
                       Test_Dispatch'Access);
   end Add_Tests;

   type Action_Bean is new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Count    : Natural := 0;
      Priority : Integer := 0;
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

   package Event_Action_Binding is
     new AWA.Events.Action_Method.Bind (Bean => Action_Bean,
                                        Method => Event_Action,
                                        Name   => "send");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Event_Action_Binding.Proxy'Access);

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Action_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Action_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "priority" then
         From.Priority := Util.Beans.Objects.To_Integer (Value);
      end if;
   end Set_Value;

   overriding
   function Get_Method_Bindings (From : in Action_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

   procedure Event_Action (From  : in out Action_Bean;
                           Event : in AWA.Events.Module_Event'Class) is
   begin
      Log.Info ("Event action called {0}", Event.Get_Parameter ("priority"));

      From.Count := From.Count + 1;
   end Event_Action;

   function Create_Action_Bean return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Action_Bean_Access := new Action_Bean;
   begin
      return Result.all'Access;
   end Create_Action_Bean;

   --  ------------------------------
   --  Test searching an event name in the definition list.
   --  ------------------------------
   procedure Test_Find_Event (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, Integer (Event_Test_4.Kind),
                                Integer (Find_Event_Index ("event-test-4")), "Find_Event");
      Util.Tests.Assert_Equals (T, Integer (Event_Test_5.Kind),
                                Integer (Find_Event_Index ("event-test-5")), "Find_Event");
      Util.Tests.Assert_Equals (T, Integer (Event_Test_1.Kind),
                                Integer (Find_Event_Index ("event-test-1")), "Find_Event");
   end Test_Find_Event;

   --  ------------------------------
   --  Test creation and initialization of event manager.
   --  ------------------------------
   procedure Test_Initialize (T : in out Test) is
      App     : AWA.Applications.Application_Access := AWA.Tests.Get_Application;
      Manager : Event_Manager;
   begin
      Manager.Initialize (App.all'Access);
--        T.Assert (Manager.Actions /= null, "Initialization failed");
   end Test_Initialize;

   --  ------------------------------
   --  Test adding an action.
   --  ------------------------------
   procedure Test_Add_Action (T : in out Test) is
      use AWA.Events.Queues;

      App     : AWA.Applications.Application_Access := AWA.Tests.Get_Application;
      Manager : Event_Manager;
      Ctx     : EL.Contexts.Default.Default_Context;
      Action  : EL.Expressions.Method_Expression := EL.Expressions.Create_Expression ("#{a.send}", Ctx);
      Props   : EL.Beans.Param_Vectors.Vector;
      Pos     : Event_Index := Find_Event_Index ("event-test-4");
      Queue   : Queue_Ref;
   begin
      Manager.Initialize (App.all'Access);

      Queue := AWA.Events.Queues.Create_Queue ("test", "fifo", Props, Ctx);
      Manager.Add_Queue (Queue);
      for I in 1 .. 10 loop
         Manager.Add_Action (Event  => "event-test-4",
                             Queue  => Queue,
                             Action => Action,
                             Params => Props);
--           Util.Tests.Assert_Equals (T, 1, Integer (Manager.Actions (Pos).Queues.Length),
--                                     "Add_Action failed");
      end loop;

   end Test_Add_Action;

   --  Test dispatching events
   procedure Test_Dispatch (T : in out Test) is

      Factory  : AWA.Applications.Factory.Application_Factory;
      Conf  : ASF.Applications.Config;
      App   : aliased AWA.Applications.Application;
      Ctx   : aliased EL.Contexts.Default.Default_Context;
      Path  : constant String := Util.Tests.Get_Test_Path ("regtests/config/event-test.xml");
      Action : aliased Action_Bean;
   begin
      Conf.Set ("database", Util.Tests.Get_Parameter ("database"));
      App.Initialize (Conf    => Conf,
                      Factory => Factory);
      App.Set_Global ("event_test",
        Util.Beans.Objects.To_Object (Action'Unchecked_Access,
          Util.Beans.Objects.STATIC));

      AWA.Applications.Configs.Read_Configuration (App     => App,
                                                   File    => Path,
                                                   Context => Ctx'Unchecked_Access);

      declare
         T : Util.Measures.Stamp;
      begin
         for I in 1 .. 100 loop
            declare
               Event : Module_Event;
            begin
               Event.Set_Event_Kind (Event_Test_4.Kind);
               Event.Set_Parameter ("prio", "3");
               Event.Set_Parameter ("template", "def");
               App.Send_Event (Event);

               Event.Set_Event_Kind (Event_Test_1.Kind);
               App.Send_Event (Event);
            end;
         end loop;
         Util.Measures.Report (T, "Send 200 events");
         Log.Info ("Action count: {0}", Natural'Image (Action.Count));
         Log.Info ("Priority: {0}", Integer'Image (Action.Priority));
      end;
   end Test_Dispatch;

end AWA.Events.Tests;
