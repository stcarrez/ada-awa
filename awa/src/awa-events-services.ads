-----------------------------------------------------------------------
--  awa-events-services -- AWA Event Manager
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
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with Util.Strings;

with EL.Expressions;
with EL.Beans;

with ADO.Sessions;

with AWA.Queues.Models;
with AWA.Events.Queues;
with AWA.Events.Dispatchers;

package AWA.Events.Services is

   --  ------------------------------
   --  Event manager
   --  ------------------------------
   --  The <b>Event_Manager</b> manages the dispatch of event to the right event queue
   --  or to the event action.  The event manager holds a list of actions that must be
   --  triggered for a particular event/queue pair.  Such list is created and initialized
   --  when the application is configured.  It never changes.
   type Event_Manager is tagged limited private;
   type Event_Manager_Access is access all Event_Manager'Class;

   --  Find the event queue identified by the given name.
   function Find_Queue (Manager : in Event_Manager;
                        Name    : in String) return AWA.Events.Queues.Queue_Access;

   --  Add the event queue in the registry.
   procedure Add_Queue (Manager : in out Event_Manager;
                        Queue   : in AWA.Events.Queues.Queue_Access);

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
                       Queue   : in AWA.Events.Queues.Queue_Access;
                       Event   : in Module_Event'Class);

   --  Add an action invoked when the event identified by <b>Event</b> is sent.
   --  The event is posted on the queue identified by <b>Queue</b>.
   --  When the event queue dispatches the event, the Ada bean identified by the method action
   --  represented by <b>Action</b> is created and initialized by evaluating and setting the
   --  parameters defined in <b>Params</b>.  The action method is then invoked.
   procedure Add_Action (Manager : in out Event_Manager;
                         Event   : in String;
                         Queue   : in AWA.Events.Queues.Queue_Access;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector);

   --  Initialize the event manager.
   procedure Initialize (Manager : in out Event_Manager;
                         DB      : in out ADO.Sessions.Master_Session);

private

   use AWA.Events.Queues;

   --  The list of queues created for the application.
   package Queue_Map is
      new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                 Element_Type    => AWA.Events.Queues.Queue_Access,
                                                 Hash            => Ada.Strings.Hash,
                                                 Equivalent_Keys => "=",
                                                 "="             => "=");

   --  An event queue associated with a dispatcher.
   type Queue_Dispatcher is record
      Queue      : AWA.Events.Queues.Queue_Access := null;
      Dispatcher : AWA.Events.Dispatchers.Dispatcher_Access := null;
   end record;

   --  A list of event queue dispatcher.
   package Queue_Dispatcher_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Queue_Dispatcher);

   --  A list of event queues for a given event.
   type Event_Queues is record
      Queues : Queue_Dispatcher_Lists.List;
      Name   : Util.Strings.Name_Access;
      Event  : AWA.Queues.Models.Message_Type_Ref;
   end record;

   --  An array of event queue actions.
   type Event_Queues_Array is array (Event_Index range <>) of Event_Queues;
   type Event_Queues_Array_Access is access all Event_Queues_Array;

   type Event_Manager is new Ada.Finalization.Limited_Controlled with record
      Actions : Event_Queues_Array_Access := null;
      Queues  : Queue_Map.Map;
   end record;

   --  Finalize the event manager by releasing the allocated storage.
   overriding
   procedure Finalize (Manager : in out Event_Manager);

end AWA.Events.Services;
