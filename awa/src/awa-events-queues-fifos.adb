-----------------------------------------------------------------------
--  awa-events-queues-fifos -- Fifo event queues (memory based)
--  Copyright (C) 2012, 2022 Stephane Carrez
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

with Util.Log.Loggers;
package body AWA.Events.Queues.Fifos is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Events.Queues.Fifos");

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => AWA.Events.Module_Event'Class,
                                      Name   => AWA.Events.Module_Event_Access);

   --  ------------------------------
   --  Get the queue name.
   --  ------------------------------
   overriding
   function Get_Name (From : in Fifo_Queue) return String is
   begin
      return From.Name;
   end Get_Name;

   --  ------------------------------
   --  Get the model queue reference object.
   --  Returns a null object if the queue is not persistent.
   --  ------------------------------
   overriding
   function Get_Queue (From : in Fifo_Queue) return AWA.Events.Models.Queue_Ref is
      pragma Unreferenced (From);
   begin
      return AWA.Events.Models.Null_Queue;
   end Get_Queue;

   --  ------------------------------
   --  Queue the event.
   --  ------------------------------
   overriding
   procedure Enqueue (Into  : in out Fifo_Queue;
                      Event : in AWA.Events.Module_Event'Class) is
      E : constant Module_Event_Access := Copy (Event);
   begin
      Log.Debug ("Enqueue event on queue {0}", Into.Name);

      E.Set_Event_Kind (Event.Get_Event_Kind);
      Into.Fifo.Enqueue (E);
   end Enqueue;

   --  ------------------------------
   --  Dequeue an event and process it with the <b>Process</b> procedure.
   --  ------------------------------
   overriding
   procedure Dequeue (From    : in out Fifo_Queue;
                      Process : access procedure (Event : in Module_Event'Class)) is
      E : Module_Event_Access;
   begin
      Log.Debug ("Dequeue event queue {0}", From.Name);

      From.Fifo.Dequeue (E, 0.0);
      begin
         Process (E.all);

      exception
         when E : others =>
            Log.Error ("Exception when processing event", E);
      end;
      Free (E);

   exception
      when Fifo_Protected_Queue.Timeout =>
         null;
   end Dequeue;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Fifo_Queue;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (From, Name);
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Fifo_Queue;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "size" then
         From.Fifo.Set_Size (Util.Beans.Objects.To_Integer (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Release the queue storage.
   --  ------------------------------
   overriding
   procedure Finalize (From : in out Fifo_Queue) is
   begin
      while From.Fifo.Get_Count > 0 loop
         declare
            E : Module_Event_Access;
         begin
            From.Fifo.Dequeue (E);
            Free (E);
         end;
      end loop;
   end Finalize;

   --  ------------------------------
   --  Create the queue associated with the given name and configure it by using
   --  the configuration properties.
   --  ------------------------------
   function Create_Queue (Name    : in String;
                          Props   : in EL.Beans.Param_Vectors.Vector;
                          Context : in EL.Contexts.ELContext'Class) return Queue_Access is
      Result : constant Fifo_Queue_Access := new Fifo_Queue '(Name_Length  => Name'Length,
                                                              Name         => Name,
                                                              others       => <>);
   begin
      EL.Beans.Initialize (Result.all, Props, Context);
      return Result.all'Access;
   end Create_Queue;

end AWA.Events.Queues.Fifos;
