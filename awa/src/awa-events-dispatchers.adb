-----------------------------------------------------------------------
--  awa-events-dispatchers -- AWA Event Dispatchers
--  Copyright (C) 2012, 2017, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Events.Services;
package body AWA.Events.Dispatchers is

   --  ------------------------------
   --  Add the queue to the dispatcher.
   --  ------------------------------
   procedure Add_Queue (Manager : in out Dispatcher;
                        Queue   : in AWA.Events.Queues.Queue_Ref;
                        Added   : out Boolean) is
      pragma Unreferenced (Manager, Queue);
   begin
      Added := False;
   end Add_Queue;

   --  ------------------------------
   --  Dispatch the events from the queue.
   --  Increment the `Count` parameter to indicate the number of events
   --  that were dispatched.
   --  ------------------------------
   procedure Dispatch (Manager : in Dispatcher;
                       Queue   : in AWA.Events.Queues.Queue_Ref;
                       Count   : in out Natural) is
      procedure Process (Event : in Module_Event'Class);

      procedure Process (Event : in Module_Event'Class) is
      begin
         Count := Count + 1;
         Manager.Manager.Dispatch (Queue, Event);
      end Process;
   begin
      Queue.Dequeue (Process'Access);
   end Dispatch;

end AWA.Events.Dispatchers;
