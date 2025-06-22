-----------------------------------------------------------------------
--  awa-events-queues-fifos -- Fifo event queues (memory based)
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Concurrent.Fifos;
with Util.Beans.Basic;

with EL.Beans;
with EL.Contexts;
private package AWA.Events.Queues.Fifos is

   type Fifo_Queue (Name_Length : Natural) is limited new Queue
     and Util.Beans.Basic.Bean with private;
   type Fifo_Queue_Access is access all Fifo_Queue'Class;

   --  Get the queue name.
   overriding
   function Get_Name (From : in Fifo_Queue) return String;

   --  Get the model queue reference object.
   --  Returns a null object if the queue is not persistent.
   overriding
   function Get_Queue (From : in Fifo_Queue) return AWA.Events.Models.Queue_Ref;

   --  Queue the event.
   overriding
   procedure Enqueue (Into  : in out Fifo_Queue;
                      Event : in AWA.Events.Module_Event'Class);

   --  Dequeue an event and process it with the <b>Process</b> procedure.
   overriding
   procedure Dequeue (From    : in out Fifo_Queue;
                      Process : access procedure (Event : in Module_Event'Class));

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Fifo_Queue;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Fifo_Queue;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Release the queue storage.
   overriding
   procedure Finalize (From : in out Fifo_Queue);

   --  Create the queue associated with the given name and configure it by using
   --  the configuration properties.
   function Create_Queue (Name    : in String;
                          Props   : in EL.Beans.Param_Vectors.Vector;
                          Context : in EL.Contexts.ELContext'Class) return Queue_Access;

private

   package Fifo_Protected_Queue is
     new Util.Concurrent.Fifos (Module_Event_Access, 100, True);

   type Fifo_Queue (Name_Length : Natural) is limited new Queue
     and Util.Beans.Basic.Bean with record
      Fifo : Fifo_Protected_Queue.Fifo;
      Name : String (1 .. Name_Length);
   end record;

end AWA.Events.Queues.Fifos;
