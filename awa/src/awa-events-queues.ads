-----------------------------------------------------------------------
--  awa-events-queues -- AWA Event Queues
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
package AWA.Events.Queues is

   type Queue is limited interface;
   type Queue_Access is access all Queue'Class;

   --  Get the queue name.
   function Get_Name (From : in Queue) return String is abstract;

   --  Queue the event.
   procedure Enqueue (Into  : in out Queue;
                      Event : in AWA.Events.Module_Event'Class) is abstract;

   --  Dequeue an event and process it with the <b>Process</b> procedure.
   procedure Dequeue (From    : in out Queue;
                      Process : access procedure (Event : in Module_Event'Class)) is abstract;

end AWA.Events.Queues;
