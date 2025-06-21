-----------------------------------------------------------------------
--  awa-events-dispatchers-tasks -- AWA Event Dispatchers
--  Copyright (C) 2012, 2017, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

with Util.Concurrent.Fifos;
with AWA.Events.Queues;
with AWA.Events.Services;
with AWA.Events.Queues.Observers;
package AWA.Events.Dispatchers.Tasks is

   subtype Observer is Queues.Observers.Observer;

   type Task_Dispatcher is limited new Dispatcher and Observer with private;
   type Task_Dispatcher_Access is access all Task_Dispatcher;

   --  Start the dispatcher.
   overriding
   procedure Start (Manager : in out Task_Dispatcher);

   --  Stop the dispatcher.
   overriding
   procedure Stop (Manager : in out Task_Dispatcher);

   --  Add the queue to the dispatcher.
   overriding
   procedure Add_Queue (Manager : in out Task_Dispatcher;
                        Queue   : in AWA.Events.Queues.Queue_Ref;
                        Added   : out Boolean);

   --  Inform the dispatch that some events may have been queued.
   overriding
   procedure Update (Manager : in Task_Dispatcher;
                     Queue   : in Queues.Queue_Ref);

   overriding
   procedure Finalize (Object : in out Task_Dispatcher) renames Stop;

   function Create_Dispatcher (Service  : in AWA.Events.Services.Event_Manager_Access;
                               Match    : in String;
                               Count    : in Positive;
                               Priority : in Positive) return Dispatcher_Access;

private

   package Queue_Of_Queue is
      new Util.Concurrent.Fifos (Element_Type     => AWA.Events.Queues.Queue_Ref,
                                 Default_Size     => 10,
                                 Clear_On_Dequeue => True);

   task type Consumer is
      entry Start (D : in Task_Dispatcher_Access);
      entry Stop;
      entry Wakeup;
   end Consumer;

   type Consumer_Array is array (Positive range <>) of Consumer;
   type Consumer_Array_Access is access Consumer_Array;

   type Task_Dispatcher is limited new Dispatcher and Observer with record
      Workers     : Consumer_Array_Access;
      Queues      : Queue_Of_Queue.Fifo;
      Task_Count  : Positive;
      Match       : Ada.Strings.Unbounded.Unbounded_String;
      Priority    : Positive;
   end record;

end AWA.Events.Dispatchers.Tasks;
