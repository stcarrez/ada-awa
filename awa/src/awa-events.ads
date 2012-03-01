-----------------------------------------------------------------------
--  awa-events -- AWA Events
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

with Ada.Finalization;
with Ada.Containers.Doubly_Linked_Lists;

with Util.Events;
with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Strings;

with EL.Expressions;
with EL.Beans;

with ADO.Sessions;

with ASF.Applications;

with AWA.Queues.Models;

--  The <b>AWA.Events</b> package defines an event framework for modules to post events
--  and have Ada bean methods be invoked when these events are dispatched.  Subscription to
--  events is done through configuration files.  This allows to configure the modules and
--  integrate them together easily at configuration time.
--
--  1/ Modules define the events they can generate by instantiating the <b>Definition</b> package.
--     This is a static definition of the event.
--
--    package Event_New_User is new AWA.Events.Definition ("new-user");
--
--  2/ The module can post an event to inform other modules or the system that a particular
--  action occurred.  The module creates the event instance of type <b>Module_Event</b> and
--  populates that event with useful properties for event receivers.
--
--      Event : AWA.Events.Module_Event;
--
--      Event.Set_Event_Kind (Event_New_User.Kind);
--      Event.Set_Parameter ("email", "harry.potter@hogwarts.org");
--
--  3/ The module will post the event by using the <b>Send_Event</b> operation.
--
--    Manager.Send_Event (Event);
--
--
package AWA.Events is

   type Queue_Index is new Natural;
   type Event_Index is new Integer;

   --  ------------------------------
   --  Event kind definition
   --  ------------------------------
   --  This package must be instantiated for each event that a module can post.
   generic
      Name : String;
   package Definition is
      function Kind return Event_Index;
      pragma Inline_Always (Kind);
   end Definition;

   --  Exception raised if an event name is not found.
   Not_Found : exception;

   --  Find the event runtime index given the event name.
   --  Raises Not_Found exception if the event name is not recognized.
   function Find_Event_Index (Name : in String) return Event_Index;

   --  ------------------------------
   --  Module event
   --  ------------------------------
   type Module_Event is new Util.Events.Event and Util.Beans.Basic.Readonly_Bean with private;

   --  Set the event type which identifies the event.
   procedure Set_Event_Kind (Event : in out Module_Event;
                             Kind  : in Event_Index);

   --  Get the event type which identifies the event.
   function Get_Event_Kind (Event : in Module_Event) return Event_Index;

   --  Set a parameter on the message.
   procedure Set_Parameter (Event  : in out Module_Event;
                            Name   : in String;
                            Value  : in String);

   --  Get the parameter with the given name.
   function Get_Parameter (Event : in Module_Event;
                           Name  : in String) return String;

   --  Get the value that corresponds to the parameter with the given name.
   overriding
   function Get_Value (Event : in Module_Event;
                       Name  : in String) return Util.Beans.Objects.Object;

   --  ------------------------------
   --  Event manager
   --  ------------------------------
   --  The <b>Event_Manager</b> manages the dispatch of event to the right event queue
   --  or to the event action.  The event manager holds a list of actions that must be
   --  triggered for a particular event/queue pair.  Such list is created and initialized
   --  when the application is configured.  It never changes.
   type Event_Manager is tagged limited private;
   type Event_Manager_Access is access all Event_Manager'Class;

   --  Send the event to the modules that subscribed to it.
   --  The event is sent on each event queue.  Event queues will dispatch the event
   --  by invoking immediately or later on the <b>Dispatch</b> operation.  The synchronous
   --  or asynchronous reception of the event depends on the event queue.
   procedure Send (Manager : in Event_Manager;
                   Event   : in Module_Event'Class);

   --  Dispatch the event identified by <b>Event</b> and associated with the event
   --  queue <b>Queue</b>.  The event actions which are associated with the event are
   --  executed synchronously.
   procedure Dispatch (Manager : in Event_Manager;
                       Queue   : in Queue_Index;
                       Event   : in Module_Event'Class);

   --  Add an action invoked when the event identified by <b>Event</b> is sent.
   --  The event is posted on the queue identified by <b>Queue</b>.
   --  When the event queue dispatches the event, the Ada bean identified by the method action
   --  represented by <b>Action</b> is created and initialized by evaluating and setting the
   --  parameters defined in <b>Params</b>.  The action method is then invoked.
   procedure Add_Action (Manager : in out Event_Manager;
                         Event   : in String;
                         Queue   : in String;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector);

   --  Initialize the event manager.
   procedure Initialize (Manager : in out Event_Manager;
                         DB      : in out ADO.Sessions.Master_Session);

private

   type Module_Event is new Util.Events.Event and Util.Beans.Basic.Readonly_Bean with record
      Kind  : Event_Index;
      Props : ASF.Applications.Config;
   end record;

   --  An event action records a method expression which identifies an Ada bean and a method
   --  that must be invoked when the event is dispatched.  The Ada bean instance is populated
   --  by evaluating and setting the set of properties before calling the action method.
   type Event_Action is record
      Action     : EL.Expressions.Method_Expression;
      Properties : EL.Beans.Param_Vectors.Vector;
   end record;

   --  A list of event actions.
   package Event_Action_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => Event_Action);

   --  A list of event actions associated to a particular event queue.
   type Queue_Actions is record
      Actions : Event_Action_Lists.List;
      Queue   : AWA.Queues.Models.Queue_Ref;
   end record;

   --  A list of event queue actions.
   package Queue_Actions_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Queue_Actions);

   type Event_Queue_Actions is record
      Queues : Queue_Actions_Lists.List;
      Name   : Util.Strings.Name_Access;
      Event  : AWA.Queues.Models.Message_Type_Ref;
   end record;

   --  An array of event queue actions.
   type Event_Actions_Array is array (Event_Index range <>) of Event_Queue_Actions;
   type Event_Actions_Array_Access is access all Event_Actions_Array;

   type Event_Manager is new Ada.Finalization.Limited_Controlled with record
      Actions : Event_Actions_Array_Access := null;
   end record;

   --  Finalize the event manager by releasing the allocated storage.
   overriding
   procedure Finalize (Manager : in out Event_Manager);

end AWA.Events;
