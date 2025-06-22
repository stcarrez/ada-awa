-----------------------------------------------------------------------
--  awa-events-queues -- AWA Event Queues
--  Copyright (C) 2012, 2019, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

with Util.Serialize.Mappers;

with AWA.Events.Queues.Fifos;
with AWA.Events.Queues.Persistents;
with AWA.Events.Queues.Observers;
package body AWA.Events.Queues is

   --  ------------------------------
   --  Queue the event.
   --  ------------------------------
   procedure Enqueue (Into  : in Queue_Ref;
                      Event : in AWA.Events.Module_Event'Class) is
   begin
      if not Into.Is_Null then
         declare
            Q : constant Queue_Info_Accessor := Into.Value;
         begin
            if Q.Queue /= null then
               Q.Queue.Enqueue (Event);
               Observers.Notify (Q.Listeners, Into);
            end if;
         end;
      end if;
   end Enqueue;

   --  ------------------------------
   --  Dequeue an event and process it with the <b>Process</b> procedure.
   --  ------------------------------
   procedure Dequeue (From    : in Queue_Ref;
                      Process : access procedure (Event : in Module_Event'Class)) is
   begin
      if not From.Is_Null then
         declare
            Q : constant Queue_Info_Accessor := From.Value;
         begin
            if Q.Queue /= null then
               From.Value.Queue.Dequeue (Process);
            end if;
         end;
      end if;
   end Dequeue;

   --  ------------------------------
   --  Add a listener that will be called each time an event is queued.
   --  ------------------------------
   procedure Add_Listener (Into     : in Queue_Ref;
                           Listener : in Util.Listeners.Listener_Access) is
   begin
      if not Into.Is_Null then
         declare
            Q : constant Queue_Info_Accessor := Into.Value;
         begin
            Q.Listeners.Append (Listener);
         end;
      end if;
   end Add_Listener;

   --  ------------------------------
   --  Returns true if the queue is available.
   --  ------------------------------
   function Has_Queue (Queue : in Queue_Ref'Class) return Boolean is
   begin
      return not Queue.Is_Null and then Queue.Value.Queue /= null;
   end Has_Queue;

   --  ------------------------------
   --  Returns the queue name.
   --  ------------------------------
   function Get_Name (Queue : in Queue_Ref'Class) return String is
   begin
      if Queue.Is_Null then
         return "";
      else
         return Queue.Value.Name;
      end if;
   end Get_Name;

   --  ------------------------------
   --  Get the model queue reference object.
   --  Returns a null object if the queue is not persistent.
   --  ------------------------------
   function Get_Queue (Queue : in Queue_Ref'Class) return AWA.Events.Models.Queue_Ref is
   begin
      if Queue.Is_Null or else Queue.Value.Queue = null then
         return AWA.Events.Models.Null_Queue;
      else
         return Queue.Value.Queue.Get_Queue;
      end if;
   end Get_Queue;

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
      Q      : constant Queue_Info_Access
        := new Queue_Info '(Util.Refs.Ref_Entity with
                            Length => Name'Length,
                            Name   => Name,
                            others => <>);
   begin
      Queue_Refs.Ref (Result) := Queue_Refs.Create (Q);
      if Kind = FIFO_QUEUE_TYPE then
         Q.Queue := Queues.Fifos.Create_Queue (Name, Props, Context);
      elsif Kind = PERSISTENT_QUEUE_TYPE then
         Q.Queue := Queues.Persistents.Create_Queue (Name, Props, Context);
      else
         raise Util.Serialize.Mappers.Field_Error with "Invalid queue type: " & Kind;
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
      if Object.Queue /= null then
         Object.Queue.Finalize;
         Free (Object.Queue);
      end if;
   end Finalize;

end AWA.Events.Queues;
