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

with Util.Concurrent.Fifos;
with AWA.Events.Queues;
package AWA.Events.Dispatchers.Tasks is


   type Task_Dispatcher is limited new Dispatcher with private;
   type Task_Dispatcher_Access is access all Task_Dispatcher;


   procedure Add_Queue (Manager : in out Task_Dispatcher;
                        Queue   : in AWA.Events.Queues.Queue_Ref);

private

   package Queue_Of_Queue is
      new Util.Concurrent.Fifos (Element_Type     => AWA.Events.Queues.Queue_Ref,
                                 Default_Size     => 10,
                                 Clear_On_Dequeue => True);

   task type Consumer is
      entry Start (D : in Task_Dispatcher_Access);
   end Consumer;

   type Task_Dispatcher is limited new Dispatcher with record
      Worker : Consumer;
      Queues : Queue_Of_Queue.Fifo;
   end record;

end AWA.Events.Dispatchers.Tasks;
