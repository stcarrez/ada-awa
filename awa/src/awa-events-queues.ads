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

with Util.Refs;

with EL.Beans;
with EL.Contexts;
package AWA.Events.Queues is

   type Queue_Ref is tagged private;

   --  Queue the event.
   procedure Enqueue (Into  : in Queue_Ref;
                      Event : in AWA.Events.Module_Event'Class);

   --  Dequeue an event and process it with the <b>Process</b> procedure.
   procedure Dequeue (From    : in Queue_Ref;
                      Process : access procedure (Event : in Module_Event'Class));

   --  Returns true if the reference does not contain any element.
   function Is_Null (Queue : in Queue_Ref'Class) return Boolean;
   --  pragma Inline_Always (Is_Null);  SCz 2012-03-17: disabled since gnat crashes.

   --  Returns the queue name.
   function Get_Name (Queue : in Queue_Ref'Class) return String;

   --  Create the event queue identified by the name <b>Name</b>.  The queue factory
   --  identified by <b>Kind</b> is called to create the event queue instance.
   --  Returns a reference to the queue.
   function Create_Queue (Name    : in String;
                          Kind    : in String;
                          Props   : in EL.Beans.Param_Vectors.Vector;
                          Context : in EL.Contexts.ELContext'Class)
                          return Queue_Ref;

   function Null_Queue return Queue_Ref;

private

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

   type Queue_Info (Length : Natural) is new Util.Refs.Ref_Entity with record
      Queue : Queue_Access := null;
      Name  : String (1 .. Length);
   end record;
   type Queue_Info_Access is access all Queue_Info;

   --  Finalize the referenced object.  This is called before the object is freed.
   overriding
   procedure Finalize (Object : in out Queue_Info);

   package Queue_Refs is
     new Util.Refs.Indefinite_References (Queue_Info, Queue_Info_Access);

   type Queue_Ref is new Queue_Refs.Ref with null record;

end AWA.Events.Queues;
