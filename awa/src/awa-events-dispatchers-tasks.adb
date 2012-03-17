-----------------------------------------------------------------------
--  awa-events-dispatchers-tasks -- AWA Event Dispatchers
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

with AWA.Events.Queues;
package body AWA.Events.Dispatchers.Tasks is


   procedure Add_Queue (Manager : in out Task_Dispatcher;
                        Queue   : in AWA.Events.Queues.Queue_Ref) is
   begin
      Manager.Queues.Enqueue (Queue);
   end Add_Queue;

   task body Consumer is
      Dispatcher : Task_Dispatcher_Access;
      Time : Duration := 0.01;
   begin
      accept Start (D : in Task_Dispatcher_Access) do
         Dispatcher := D;
      end Start;
      loop
         declare
            Nb_Queues : constant Natural := Dispatcher.Queues.Get_Count;
            Queue     : AWA.Events.Queues.Queue_Ref;
            Nb_Events : Natural := 0;
         begin
            --  We can have several tasks that dispatch events from several queues.
            --  Each queue in the list must be given the same polling quota.
            --  Pick a queue and dispatch some pending events.
            --  Put back the queue in the fifo.
            for I in 1 .. Nb_Queues loop
               Dispatcher.Queues.Dequeue (Queue);

               Dispatcher.Queues.Enqueue (Queue);
            end loop;

            --  If we processed something, reset the timeout delay and continue polling.
            --  Otherwise, double the sleep time.
            if Nb_Events /= 0 then
               Time := 0.01;
            else
               delay Time;
               if Time < 60.0 then
                  Time := Time * 2.0;
               end if;
            end if;
         end;
      end loop;
   end Consumer;

end AWA.Events.Dispatchers.Tasks;
