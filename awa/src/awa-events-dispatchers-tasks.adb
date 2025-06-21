-----------------------------------------------------------------------
--  awa-events-dispatchers-tasks -- AWA Event Dispatchers
--  Copyright (C) 2012, 2015, 2017, 2019, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

with Util.Log.Loggers;

with AWA.Services.Contexts;
package body AWA.Events.Dispatchers.Tasks is

   use Util.Log;
   use Ada.Strings.Unbounded;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Events.Dispatchers.Tasks");

   --  ------------------------------
   --  Start the dispatcher.
   --  ------------------------------
   overriding
   procedure Start (Manager : in out Task_Dispatcher) is
   begin
      if Manager.Queues.Get_Count > 0 then
         Log.Info ("Starting the tasks");

         if Manager.Workers = null then
            Manager.Workers := new Consumer_Array (1 .. Manager.Task_Count);
         end if;

         for I in Manager.Workers'Range loop
            Manager.Workers (I).Start (Manager'Unchecked_Access);
         end loop;
      else
         Log.Info ("No event dispatcher task started (no event queue to poll)");
      end if;
   end Start;

   --  ------------------------------
   --  Stop the dispatcher.
   --  ------------------------------
   overriding
   procedure Stop (Manager : in out Task_Dispatcher) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Consumer_Array,
                                        Name   => Consumer_Array_Access);
   begin
      if Manager.Workers /= null then
         Log.Info ("Stopping the event dispatcher tasks");

         for I in Manager.Workers'Range loop
            if Manager.Workers (I)'Callable then
               Manager.Workers (I).Stop;
            else
               Log.Error ("Event consumer task terminated abnormally");
            end if;
         end loop;
         Free (Manager.Workers);
      end if;
   end Stop;

   --  ------------------------------
   --  Inform the dispatch that some events may have been queued.
   --  ------------------------------
   overriding
   procedure Update (Manager : in Task_Dispatcher;
                     Queue   : in Queues.Queue_Ref) is
      pragma Unreferenced (Queue);
   begin
      if Manager.Workers /= null then
         Log.Debug ("Wakeup dispatcher tasks");

         for I in Manager.Workers'Range loop
            if Manager.Workers (I)'Callable then
               Manager.Workers (I).Wakeup;
            else
               Log.Error ("Event consumer task terminated abnormally");
            end if;
         end loop;
      end if;
   end Update;

   overriding
   procedure Add_Queue (Manager : in out Task_Dispatcher;
                        Queue   : in AWA.Events.Queues.Queue_Ref;
                        Added   : out Boolean) is
   begin
      Log.Info ("Adding queue {0} to the task dispatcher", Queue.Get_Name);

      Manager.Queues.Enqueue (Queue);
      Queue.Add_Listener (Manager'Unchecked_Access);
      Added := True;
   end Add_Queue;

   function Create_Dispatcher (Service  : in AWA.Events.Services.Event_Manager_Access;
                               Match    : in String;
                               Count    : in Positive;
                               Priority : in Positive) return Dispatcher_Access is
      Result : constant Task_Dispatcher_Access := new Task_Dispatcher;
   begin
      Result.Task_Count := Count;
      Result.Priority   := Priority;
      Result.Match      := To_Unbounded_String (Match);
      Result.Manager    := Service.all'Access;
      return Result.all'Access;
   end Create_Dispatcher;

   task body Consumer is
      Dispatcher : Task_Dispatcher_Access;
      Time       : Duration := 0.01;
      Do_Work    : Boolean := True;
   begin
      Log.Info ("Event consumer is ready");
      select
         accept Start (D : in Task_Dispatcher_Access) do
            Dispatcher := D;
         end Start;
         Log.Info ("Event consumer is started");

         while Do_Work loop
            declare
               Nb_Queues : constant Natural := Dispatcher.Queues.Get_Count;
               Queue     : AWA.Events.Queues.Queue_Ref;
               Nb_Events : Natural := 0;
               Context   : AWA.Services.Contexts.Service_Context;
            begin
               --  Set the service context.
               Context.Set_Context (Application => Dispatcher.Manager.Get_Application.all'Access,
                                    Principal   => null);

               --  We can have several tasks that dispatch events from several queues.
               --  Each queue in the list must be given the same polling quota.
               --  Pick a queue and dispatch some pending events.
               --  Put back the queue in the fifo.
               for I in 1 .. Nb_Queues loop
                  Dispatcher.Queues.Dequeue (Queue);
                  begin
                     Dispatcher.Dispatch (Queue, Nb_Events);

                  exception
                     when E : others =>
                        Log.Error ("Exception when dispatching events", E, True);
                  end;
                  Dispatcher.Queues.Enqueue (Queue);
               end loop;

               --  If we processed something, reset the timeout delay and continue polling.
               --  Otherwise, double the sleep time.
               if Nb_Events /= 0 then
                  Time := 0.01;
               else
                  Log.Debug ("Sleeping {0} seconds", Duration'Image (Time));

                  select
                     accept Stop do
                        Do_Work := False;
                     end Stop;
                  or
                     accept Wakeup do
                        Do_Work := True;
                     end Wakeup;
                  or
                     accept Start (D : in Task_Dispatcher_Access) do
                        Dispatcher := D;
                     end Start;
                  or
                     delay Time;
                  end select;

                  if Time < 60.0 then
                     Time := Time * 2.0;
                  end if;
               end if;
            end;
         end loop;
      or
         terminate;
      end select;
   end Consumer;

end AWA.Events.Dispatchers.Tasks;
