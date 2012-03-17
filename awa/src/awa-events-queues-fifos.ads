-----------------------------------------------------------------------
--  awa-events-queues-fifos -- Fifo event queues (memory based)
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

with EL.Beans;
with EL.Contexts;
private package AWA.Events.Queues.Fifos is

   type Fifo_Queue (Name_Length : Natural) is limited new Queue with private;
   type Fifo_Queue_Access is access all Fifo_Queue'Class;

   --  Get the queue name.
   function Get_Name (From : in Fifo_Queue) return String;

   --  Queue the event.
   procedure Enqueue (Into  : in out Fifo_Queue;
                      Event : in AWA.Events.Module_Event'Class);

   --  Dequeue an event and process it with the <b>Process</b> procedure.
   procedure Dequeue (From    : in out Fifo_Queue;
                      Process : access procedure (Event : in Module_Event'Class));

   --  Create the queue associated with the given name and configure it by using
   --  the configuration properties.
   function Create_Queue (Name    : in String;
                          Props   : in EL.Beans.Param_Vectors.Vector;
                          Context : in EL.Contexts.ELContext'Class) return Queue_Access;

private

   package Fifo_Protected_Queue is
     new Util.Concurrent.Fifos (Module_Event_Access, 100, True);

   type Fifo_Queue (Name_Length : Natural) is limited new Queue with record
      Fifo : Fifo_Protected_Queue.Fifo;
      Name : String (1 .. Name_Length);
   end record;

end AWA.Events.Queues.Fifos;
