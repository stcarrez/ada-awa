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
with Ada.Unchecked_Deallocation;

with Util.Serialize.Mappers;

with AWA.Events.Queues.Fifos;
with AWA.Events.Queues.Persistents;
package body AWA.Events.Queues is

   --  ------------------------------
   --  Queue the event.
   --  ------------------------------
   procedure Enqueue (Into  : in Queue_Ref;
                      Event : in AWA.Events.Module_Event'Class) is
      Q : constant Queue_Info_Access := Into.Value;
   begin
      if Q = null or else Q.Queue = null then
         return;
      end if;
      Q.Queue.Enqueue (Event);
   end Enqueue;

   --  ------------------------------
   --  Dequeue an event and process it with the <b>Process</b> procedure.
   --  ------------------------------
   procedure Dequeue (From    : in Queue_Ref;
                      Process : access procedure (Event : in Module_Event'Class)) is
      Q : constant Queue_Info_Access := From.Value;
   begin
      if Q = null or else Q.Queue = null then
         return;
      end if;
      Q.Queue.Dequeue (Process);
   end Dequeue;

   --  ------------------------------
   --  Returns true if the reference does not contain any element.
   --  ------------------------------
   function Is_Null (Queue : in Queue_Ref'Class) return Boolean is
      Q : constant Queue_Info_Access := Queue.Value;
   begin
      return Q = null or else Q.Queue = null;
   end Is_Null;

   --  ------------------------------
   --  Returns the queue name.
   --  ------------------------------
   function Get_Name (Queue : in Queue_Ref'Class) return String is
      Q : constant Queue_Info_Access := Queue.Value;
   begin
      if Q = null then
         return "";
      else
         return Q.Name;
      end if;
   end Get_Name;

   FIFO_QUEUE_TYPE       : constant String := "fifo";
   PERSISTENT_QUEUE_TYPE : constant String := "persist";

   --  ------------------------------
   --  Create the event queue identified by the name <b>Name</b>.  The queue factory
   --  identified by <b>Kind</b> is called to create the event queue instance.
   --  Returns a reference to the queue.
   --  ------------------------------
   function Create_Queue (Name    : in String;
                          Kind    : in String;
                          Props   : in EL.Beans.Param_Vectors.Vector;
                          Context : in EL.Contexts.ELContext'Class)
                          return Queue_Ref is
      Result : Queue_Ref;
      Q      : constant Queue_Info_Access := new Queue_Info '(Util.Refs.Ref_Entity with
                                                              Length => Name'Length,
                                                              Name   => Name,
                                                              others => <>);
   begin
      Queue_Refs.Ref (Result) := Queue_Refs.Create (Q);
      if Kind = FIFO_QUEUE_TYPE then
         Q.Queue := AWA.Events.Queues.Fifos.Create_Queue (Name, Props, Context);
      elsif Name = PERSISTENT_QUEUE_TYPE then
         Q.Queue := AWA.Events.Queues.Persistents.Create_Queue (Name, Props, Context);
      else
         raise Util.Serialize.Mappers.Field_Error with "Invalid queue type: " & Name;
      end if;
      return Result;
   end Create_Queue;

   function Null_Queue return Queue_Ref is
      Result : Queue_Ref;
   begin
      return Result;
   end Null_Queue;

   --  ------------------------------
   --  Finalize the referenced object.  This is called before the object is freed.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Queue_Info) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Queue'Class,
                                         Name   => Queue_Access);
   begin
      Free (Object.Queue);
   end Finalize;

end AWA.Events.Queues;
