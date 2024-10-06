-----------------------------------------------------------------------
--  events-tests -- Unit tests for AWA events
--  Copyright (C) 2012, 2015, 2019, 2021, 2022 Stephane Carrez
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

with Util.Test_Caller;
with Util.Log.Loggers;
with Util.Measures;
with Util.Concurrent.Counters;

with EL.Beans;
with EL.Expressions;
with EL.Contexts.Default;

with ASF.Applications;
with ASF.Servlets.Faces;

with AWA.Applications;
with AWA.Applications.Configs;
with AWA.Applications.Factory;
with AWA.Events.Action_Method;
with AWA.Services.Contexts;

with AWA.Events.Queues;

package body AWA.Events.Services.Tests is

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
      Caller.Add_Test (Suite, "Test AWA.Events.Get_Event_Name",
                       Test_Get_Event_Name'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Find_Event_Index",
                       Test_Find_Event'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Initialize",
                       Test_Initialize'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Add_Action",
                       Test_Add_Action'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Dispatch_Synchronous",
                       Test_Dispatch_Synchronous'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Dispatch_Fifo",
                       Test_Dispatch_Fifo'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Dispatch_Persist",
                       Test_Dispatch_Persist'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Dispatch_Synchronous_Dyn",
                       Test_Dispatch_Synchronous_Dyn'Access);
      Caller.Add_Test (Suite, "Test AWA.Events.Dispatch_Synchronous_Raise",
                       Test_Dispatch_Synchronous_Raise'Access);
   end Add_Tests;

   package Event_Action_Binding is
     new AWA.Events.Action_Method.Bind (Bean => Action_Bean,
                                        Method => Event_Action,
                                        Name   => "send");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Event_Action_Binding.Proxy'Access);

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Action_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (From, Name);
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Action_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "priority" then
         From.Priority := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "raise_exception" then
         From.Raise_Exception := Util.Beans.Objects.To_Boolean (Value);
      end if;
   end Set_Value;

   overriding
   function Get_Method_Bindings (From : in Action_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

   Action_Exception : exception;
   Global_Counter   : Util.Concurrent.Counters.Counter;

   procedure Event_Action (From  : in out Action_Bean;
                           Event : in AWA.Events.Module_Event'Class) is
      pragma Unreferenced (Event);
   begin
      if From.Raise_Exception then
         raise Action_Exception with "Raising an exception from the event action bean";
      end if;
      From.Count := From.Count + 1;
      Util.Concurrent.Counters.Increment (Global_Counter);
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
   --  Test the Get_Event_Type_Name internal operation.
   --  ------------------------------
   procedure Test_Get_Event_Name (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "event-test-1", Get_Event_Type_Name (Event_Test_1.Kind).all,
                                "Get_Event_Type_Name");
      Util.Tests.Assert_Equals (T, "event-test-2", Get_Event_Type_Name (Event_Test_2.Kind).all,
                                "Get_Event_Type_Name");
      Util.Tests.Assert_Equals (T, "event-test-3", Get_Event_Type_Name (Event_Test_3.Kind).all,
                                "Get_Event_Type_Name");
      Util.Tests.Assert_Equals (T, "event-test-4", Get_Event_Type_Name (Event_Test_4.Kind).all,
                                "Get_Event_Type_Name");
      Util.Tests.Assert_Equals (T, "event-test-5", Get_Event_Type_Name (Event_Test_5.Kind).all,
                                "Get_Event_Type_Name");
   end Test_Get_Event_Name;

   --  ------------------------------
   --  Test creation and initialization of event manager.
   --  ------------------------------
   procedure Test_Initialize (T : in out Test) is
      App     : constant AWA.Applications.Application_Access := AWA.Tests.Get_Application;
      Manager : Event_Manager;
   begin
      Manager.Initialize (App.all'Access);
      T.Assert (Manager.Actions /= null, "Initialization failed");
   end Test_Initialize;

   --  ------------------------------
   --  Test adding an action.
   --  ------------------------------
   procedure Test_Add_Action (T : in out Test) is
      App     : constant AWA.Applications.Application_Access := AWA.Tests.Get_Application;
      Manager : Event_Manager;
      Ctx     : EL.Contexts.Default.Default_Context;
      Action  : constant EL.Expressions.Method_Expression
        := EL.Expressions.Create_Expression ("#{a.send}", Ctx);
      Props   : EL.Beans.Param_Vectors.Vector;
      Queue   : Queue_Ref;
      Index   : constant Event_Index := Find_Event_Index ("event-test-4");
   begin
      Manager.Initialize (App.all'Access);

      Queue := AWA.Events.Queues.Create_Queue ("test", "fifo", Props, Ctx);
      Manager.Add_Queue (Queue);
      for I in 1 .. 10 loop
         Manager.Add_Action (Event  => "event-test-4",
                             Queue  => Queue,
                             Action => Action,
                             Params => Props);
         Util.Tests.Assert_Equals (T, 1, Integer (Manager.Actions (Index).Queues.Length),
                                   "Add_Action failed");
      end loop;

   end Test_Add_Action;

   --  ------------------------------
   --  Test dispatching events
   --  ------------------------------
   procedure Dispatch_Event (T            : in out Test;
                             Kind         : in Event_Index;
                             Expect_Count : in Natural;
                             Expect_Prio  : in Natural) is

      Factory : AWA.Applications.Factory.Application_Factory;
      Conf    : ASF.Applications.Config;
      Ctx     : aliased EL.Contexts.Default.Default_Context;
      Path    : constant String := Util.Tests.Get_Path ("regtests/config/event-test.xml");
      Action  : aliased Action_Bean;
   begin
      Conf.Set ("database", Util.Tests.Get_Parameter ("database"));

      declare
         App   : aliased AWA.Tests.Test_Application;
         S     : Util.Measures.Stamp;
         Faces : aliased ASF.Servlets.Faces.Faces_Servlet;
         SC    : AWA.Services.Contexts.Service_Context;
      begin
         App.Initialize (Conf    => Conf,
                         Factory => Factory);
         App.Set_Global ("event_test",
           Util.Beans.Objects.To_Object (Action'Unchecked_Access,
             Util.Beans.Objects.STATIC));

         SC.Set_Context (App'Unchecked_Access, null);

         App.Add_Servlet (Name => "faces", Server => Faces'Unchecked_Access);
         App.Register_Class ("AWA.Events.Tests.Event_Action",
                             Create_Action_Bean'Access);
         AWA.Applications.Configs.Read_Configuration (App     => App,
                                                      File    => Path,
                                                      Context => Ctx'Unchecked_Access,
                                                      Override_Context => True);
         Util.Measures.Report (S, "Initialize AWA application and read config");
         App.Start;
         Util.Measures.Report (S, "Start event tasks");

         for I in 1 .. 100 loop
            declare
               Event : Module_Event;
            begin
               Event.Set_Event_Kind (Kind);
               Event.Set_Parameter ("prio", "3");
               Event.Set_Parameter ("template", "def");
               App.Send_Event (Event);
            end;
         end loop;
         Util.Measures.Report (S, "Send 100 events");

         --  Wait for the dispatcher to process the events but do not wait more than 10 secs.
         for I in 1 .. 10_000 loop
            exit when Action.Count = Expect_Count;

            delay 0.1;
         end loop;
      end;

      Log.Info ("Action count: {0}", Natural'Image (Action.Count));
      Log.Info ("Priority: {0}", Integer'Image (Action.Priority));
      Util.Tests.Assert_Equals (T, Expect_Count, Action.Count,
                                "invalid number of calls for the action (global bean)");
      Util.Tests.Assert_Equals (T, Expect_Prio, Action.Priority,
                                "prio parameter not transmitted (global bean)");
   end Dispatch_Event;

   --  ------------------------------
   --  Test dispatching synchronous event to a global bean.
   --  ------------------------------
   procedure Test_Dispatch_Synchronous (T : in out Test) is
   begin
      T.Dispatch_Event (Event_Test_1.Kind, 500, 3);
   end Test_Dispatch_Synchronous;

   --  ------------------------------
   --  Test dispatching event through a fifo queue.
   --  ------------------------------
   procedure Test_Dispatch_Fifo (T : in out Test) is
   begin
      T.Dispatch_Event (Event_Test_2.Kind, 200, 3);
   end Test_Dispatch_Fifo;

   --  ------------------------------
   --  Test dispatching event through a database queue.
   --  ------------------------------
   procedure Test_Dispatch_Persist (T : in out Test) is
   begin
      T.Dispatch_Event (Event_Test_5.Kind, 100, 3);
   end Test_Dispatch_Persist;

   --  ------------------------------
   --  Test dispatching synchronous event to a dynamic bean (created on demand).
   --  ------------------------------
   procedure Test_Dispatch_Synchronous_Dyn (T : in out Test) is
   begin
      T.Dispatch_Event (Event_Test_3.Kind, 0, 0);
   end Test_Dispatch_Synchronous_Dyn;

   --  ------------------------------
   --  Test dispatching synchronous event to a dynamic bean and raise an exception in the action.
   --  ------------------------------
   procedure Test_Dispatch_Synchronous_Raise (T : in out Test) is
   begin
      T.Dispatch_Event (Event_Test_4.Kind, 0, 0);
   end Test_Dispatch_Synchronous_Raise;

end AWA.Events.Services.Tests;
