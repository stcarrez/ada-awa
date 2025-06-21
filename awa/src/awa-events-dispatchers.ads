-----------------------------------------------------------------------
--  awa-events-dispatchers -- AWA Event Dispatchers
--  Copyright (C) 2012, 2017, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Finalization;

with EL.Expressions;
with EL.Beans;

with AWA.Events.Queues;
limited with AWA.Events.Services;
package AWA.Events.Dispatchers is

   MAX_DISPATCHER_COUNT : constant Positive := 100;

   type Dispatcher is new Ada.Finalization.Limited_Controlled with private;
   type Dispatcher_Access is access all Dispatcher'Class;
   type Dispatcher_Access_Array is array (1 .. MAX_DISPATCHER_COUNT) of Dispatcher_Access;

   --  Start the dispatcher.
   procedure Start (Manager : in out Dispatcher) is null;

   --  Stop the dispatcher.
   procedure Stop (Manager : in out Dispatcher) is null;

   --  Add the queue to the dispatcher.
   procedure Add_Queue (Manager : in out Dispatcher;
                        Queue   : in AWA.Events.Queues.Queue_Ref;
                        Added   : out Boolean);

   --  Dispatch the events from the queue.
   --  Increment the `Count` parameter to indicate the number of events
   --  that were dispatched.
   procedure Dispatch (Manager : in Dispatcher;
                       Queue   : in AWA.Events.Queues.Queue_Ref;
                       Count   : in out Natural);

   --  Dispatch the event identified by `Event`.
   --  The event actions which are associated with the event are executed
   --  synchronously.
   procedure Dispatch (Manager : in Dispatcher;
                       Event   : in Module_Event'Class) is null;

   --  Add an action invoked when an event is dispatched through this dispatcher.
   --  When the event queue dispatches the event, the Ada bean identified
   --  by the method action represented by `Action` is created and initialized
   --  by evaluating and setting the parameters defined in `Params`.
   --  The action method is then invoked.
   procedure Add_Action (Manager : in out Dispatcher;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector) is null;

private

   type Event_Manager_Access is access all AWA.Events.Services.Event_Manager'Class;

   type Dispatcher is new Ada.Finalization.Limited_Controlled with record
      Manager : Event_Manager_Access;
   end record;

end AWA.Events.Dispatchers;
