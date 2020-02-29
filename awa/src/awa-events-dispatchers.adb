-----------------------------------------------------------------------
--  awa-events-dispatchers -- AWA Event Dispatchers
--  Copyright (C) 2012, 2017, 2020 Stephane Carrez
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
