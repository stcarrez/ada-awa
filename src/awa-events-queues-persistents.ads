-----------------------------------------------------------------------
--  awa-events-queues-persistents -- Persistent event queues
--  Copyright (C) 2012, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with EL.Beans;
with EL.Contexts;

with AWA.Events.Models;
private package AWA.Events.Queues.Persistents is

   type Persistent_Queue (Name_Length : Natural) is limited new Queue with private;
   type Persistent_Queue_Access is access all Persistent_Queue'Class;

   --  Get the queue name.
   overriding
   function Get_Name (From : in Persistent_Queue) return String;

   --  Get the model queue reference object.
   --  Returns a null object if the queue is not persistent.
   overriding
   function Get_Queue (From : in Persistent_Queue) return Events.Models.Queue_Ref;

   --  Queue the event.  The event is saved in the database with a relation to
   --  the user, the user session, the event queue and the event type.
   overriding
   procedure Enqueue (Into  : in out Persistent_Queue;
                      Event : in AWA.Events.Module_Event'Class);

   overriding
   procedure Dequeue (From    : in out Persistent_Queue;
                      Process : access procedure (Event : in Module_Event'Class));

   --  Create the queue associated with the given name and configure it by using
   --  the configuration properties.
   function Create_Queue (Name    : in String;
                          Props   : in EL.Beans.Param_Vectors.Vector;
                          Context : in EL.Contexts.ELContext'Class) return Queue_Access;

private

   type Persistent_Queue (Name_Length : Natural) is limited new Queue with record
      Queue       : AWA.Events.Models.Queue_Ref;
      Server_Id   : Natural := 0;
      Max_Batch   : Positive := 1;
      Name        : String (1 .. Name_Length);
   end record;

end AWA.Events.Queues.Persistents;
