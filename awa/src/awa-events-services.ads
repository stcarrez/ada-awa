-----------------------------------------------------------------------
--  awa-events-services -- AWA Event Manager
--  Copyright (C) 2012, 2016, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Finalization;
with Ada.Containers.Doubly_Linked_Lists;

with Util.Strings;

with EL.Expressions;
with EL.Beans;

with AWA.Events.Models;
with AWA.Events.Queues;
with AWA.Events.Queues.Maps;
with AWA.Events.Dispatchers;
limited with AWA.Applications;

package AWA.Events.Services is

   --  ------------------------------
   --  Event manager
   --  ------------------------------
   --  The `Event_Manager` manages the dispatch of event to the right event queue
   --  or to the event action.  The event manager holds a list of actions that
   --  must be triggered for a particular event/queue pair.  Such list is created
   --  and initialized when the application is configured.  It never changes.
   type Event_Manager is tagged limited private;
   type Event_Manager_Access is access all Event_Manager'Class;

   --  Find the event queue identified by the given name.
   function Find_Queue (Manager : in Event_Manager;
                        Name    : in String) return AWA.Events.Queues.Queue_Ref;

   --  Add the event queue in the registry.
   procedure Add_Queue (Manager : in out Event_Manager;
                        Queue   : in AWA.Events.Queues.Queue_Ref);

   --  Send the event to the modules that subscribed to it.
   --  The event is sent on each event queue.  Event queues will dispatch the event
   --  by invoking immediately or later on the `Dispatch`> operation.
   --  The synchronous or asynchronous reception of the event depends
   --  on the event queue.
   procedure Send (Manager : in Event_Manager;
                   Event   : in Module_Event'Class);

   --  Set the event message type which correspond to the `Kind` event index.
   procedure Set_Message_Type (Manager : in Event_Manager;
                               Event   : in out AWA.Events.Models.Message_Ref;
                               Kind    : in Event_Index);

   --  Set the event queue associated with the event message.
   --  The event queue identified by `Name` is searched to find the
   --  `Queue_Ref` instance.
   procedure Set_Event_Queue (Manager : in Event_Manager;
                              Event   : in out AWA.Events.Models.Message_Ref;
                              Name    : in String);

   --  Dispatch the event identified by `Event` and associated with the event
   --  queue `Queue`.  The event actions which are associated with the event are
   --  executed synchronously.
   procedure Dispatch (Manager : in Event_Manager;
                       Queue   : in AWA.Events.Queues.Queue_Ref;
                       Event   : in Module_Event'Class);

   --  Add an action invoked when the event identified by `Event` is sent.
   --  The event is posted on the queue identified by `Queue`.
   --  When the event queue dispatches the event, the Ada bean identified
   --  by the method action represented by `Action` is created and initialized
   --  by evaluating and setting the parameters defined in `Params`.
   --  The action method is then invoked.
   procedure Add_Action (Manager : in out Event_Manager;
                         Event   : in String;
                         Queue   : in AWA.Events.Queues.Queue_Ref;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector);

   --  Add a dispatcher to process the event queues matching the `Match` string.
   --  The dispatcher can create up to `Count` tasks running at the priority
   --  `Priority`.
   procedure Add_Dispatcher (Manager  : in out Event_Manager;
                             Match    : in String;
                             Count    : in Positive;
                             Priority : in Positive);

   type Application_Access is access all AWA.Applications.Application'Class;

   --  Initialize the event manager.
   procedure Initialize (Manager : in out Event_Manager;
                         App     : in Application_Access);

   --  Start the event manager.  The dispatchers are configured to dispatch
   --  the event queues and tasks are started to process asynchronous events.
   procedure Start (Manager : in out Event_Manager);

   --  Stop the event manager.
   procedure Stop (Manager : in out Event_Manager);

   --  Get the application associated with the event manager.
   function Get_Application (Manager : in Event_Manager) return Application_Access;

private

   use AWA.Events.Queues;

   --  An event queue associated with a dispatcher.
   type Queue_Dispatcher is record
      Queue      : AWA.Events.Queues.Queue_Ref;
      Dispatcher : AWA.Events.Dispatchers.Dispatcher_Access := null;
      Model      : AWA.Events.Models.Queue_Ref;
   end record;

   --  Finalize the queue dispatcher releasing the dispatcher memory.
   procedure Finalize (Object : in out Queue_Dispatcher);

   --  A list of event queue dispatcher.
   package Queue_Dispatcher_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Queue_Dispatcher);

   --  A list of event queues for a given event.
   type Event_Queues is record
      Queues : Queue_Dispatcher_Lists.List;
      Name   : Util.Strings.Name_Access;
      Event  : AWA.Events.Models.Message_Type_Ref;
   end record;

   --  Finalize the event queues and the dispatchers.
   procedure Finalize (Object : in out Event_Queues);

   --  An array of event queue actions.
   type Event_Queues_Array is array (Event_Index range <>) of Event_Queues;
   type Event_Queues_Array_Access is access all Event_Queues_Array;

   type Event_Manager is new Ada.Finalization.Limited_Controlled with record
      Actions     : Event_Queues_Array_Access := null;
      Queues      : AWA.Events.Queues.Maps.Map;
      Application : Application_Access := null;
      Dispatchers : Events.Dispatchers.Dispatcher_Access_Array := (others => null);
   end record;

   --  Finalize the event manager by releasing the allocated storage.
   overriding
   procedure Finalize (Manager : in out Event_Manager);

end AWA.Events.Services;
