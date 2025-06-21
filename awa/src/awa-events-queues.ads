-----------------------------------------------------------------------
--  awa-events-queues -- AWA Event Queues
--  Copyright (C) 2012, 2019, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Listeners;
private with Util.Refs;

with EL.Beans;
with EL.Contexts;

with AWA.Events.Models;
package AWA.Events.Queues is

   FIFO_QUEUE_TYPE       : constant String := "fifo";
   PERSISTENT_QUEUE_TYPE : constant String := "persist";

   type Queue_Ref is tagged private;

   --  Queue the event.
   procedure Enqueue (Into  : in Queue_Ref;
                      Event : in AWA.Events.Module_Event'Class);

   --  Dequeue an event and process it with the <b>Process</b> procedure.
   procedure Dequeue (From    : in Queue_Ref;
                      Process : access procedure (Event : in Module_Event'Class));

   --  Add a listener that will be called each time an event is queued.
   procedure Add_Listener (Into     : in Queue_Ref;
                           Listener : in Util.Listeners.Listener_Access);

   --  Returns true if the queue is available.
   function Has_Queue (Queue : in Queue_Ref'Class) return Boolean;

   --  Returns the queue name.
   function Get_Name (Queue : in Queue_Ref'Class) return String;

   --  Get the model queue reference object.
   --  Returns a null object if the queue is not persistent.
   function Get_Queue (Queue : in Queue_Ref'Class) return Events.Models.Queue_Ref;

   --  Create the event queue identified by the name `Name`.  The queue factory
   --  identified by `Kind` is called to create the event queue instance.
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

   --  Get the model queue reference object.
   --  Returns a null object if the queue is not persistent.
   function Get_Queue (From : in Queue) return Events.Models.Queue_Ref is abstract;

   --  Queue the event.
   procedure Enqueue (Into  : in out Queue;
                      Event : in AWA.Events.Module_Event'Class) is abstract;

   --  Dequeue an event and process it with the `Process` procedure.
   procedure Dequeue (From    : in out Queue;
                      Process : access procedure (Event : in Module_Event'Class)) is abstract;

   --  Release the queue storage.
   procedure Finalize (From : in out Queue) is null;

   type Queue_Info (Length : Natural) is new Util.Refs.Ref_Entity with record
      Queue     : Queue_Access := null;
      Listeners : Util.Listeners.List;
      Name      : String (1 .. Length);
   end record;
   type Queue_Info_Access is access all Queue_Info;

   --  Finalize the referenced object.  This is called before the object is freed.
   overriding
   procedure Finalize (Object : in out Queue_Info);

   package Queue_Refs is
     new Util.Refs.Indefinite_References (Queue_Info, Queue_Info_Access);

   subtype Queue_Info_Accessor is Queue_Refs.Element_Accessor;

   type Queue_Ref is new Queue_Refs.Ref with null record;

end AWA.Events.Queues;
