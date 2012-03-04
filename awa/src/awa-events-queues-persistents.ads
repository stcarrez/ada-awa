-----------------------------------------------------------------------
--  awa-events-queues-persistents -- AWA Event Queues
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
with Util.Concurrent.Fifos;
with AWA.Events.Models;
package AWA.Events.Queues.Persistents is

   type Persistent_Queue (Name_Length : Natural) is limited new Queue with private;

   --  Get the queue name.
   function Get_Name (From : in Persistent_Queue) return String;

   procedure Enqueue (Into  : in out Persistent_Queue;
                      Event : in AWA.Events.Module_Event'Class);

   procedure Dequeue (From    : in out Persistent_Queue;
                      Process : access procedure (Event : in Module_Event'Class));

private

   package Fifo_Protected_Queue is
     new Util.Concurrent.Fifos (Module_Event_Access, 100, True);

   type Persistent_Queue (Name_Length : Natural) is
     new Fifo_Protected_Queue.Fifo and Queue with record
      Name  : String (1 .. Name_Length);
      Queue : AWA.Events.Models.Queue_Ref;
   end record;

end AWA.Events.Queues.Persistents;
