-----------------------------------------------------------------------
--  awa-events-services -- AWA Event Manager
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

with Util.Log.Loggers;

with ADO.SQL;

with AWA.Events.Dispatchers.Actions;
package body AWA.Events.Services is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Events");

   --  ------------------------------
   --  Send the event to the modules that subscribed to it.
   --  The event is sent on each event queue.  Event queues will dispatch the event
   --  by invoking immediately or later on the <b>Dispatch</b> operation.  The synchronous
   --  or asynchronous reception of the event depends on the event queue.
   --  ------------------------------
   procedure Send (Manager : in Event_Manager;
                   Event   : in Module_Event'Class) is

      procedure Send_Queue (Queue : in Queue_Dispatcher) is
      begin
         Queue.Queue.Enqueue (Event);
      end Send_Queue;

   begin
      if Event.Kind = Invalid_Event or Event.Kind > Manager.Actions'Last then
         Log.Error ("Cannot send event type {0}", Event_Index'Image (Event.Kind));
         raise Not_Found;
      end if;

      --  Find the event queues associated with the event.  Post the event on each queue.
      --  Some queue can dispatch the event immediately while some others may dispatched it
      --  asynchronously.
      declare
         Pos  : Queue_Dispatcher_Lists.Cursor := Manager.Actions (Event.Kind).Queues.First;
      begin
         while not Queue_Dispatcher_Lists.Has_Element (Pos) loop
            Queue_Dispatcher_Lists.Query_Element (Pos, Send_Queue'Access);
            Queue_Dispatcher_Lists.Next (Pos);
         end loop;
      end;
   end Send;

   --  ------------------------------
   --  Dispatch the event identified by <b>Event</b> and associated with the event
   --  queue <b>Queue</b>.  The event actions which are associated with the event are
   --  executed synchronously.
   --  ------------------------------
   procedure Dispatch (Manager : in Event_Manager;
                       Queue   : in AWA.Events.Queues.Queue_Access;
                       Event   : in Module_Event'Class) is

      Found : Boolean := False;

      procedure Find_Queue (List : in Queue_Dispatcher) is
      begin
         if List.Queue = Queue then
            List.Dispatcher.Dispatch (Event);
            Found := True;
         end if;
      end Find_Queue;

   begin
      if Event.Kind = Invalid_Event or Event.Kind > Manager.Actions'Last then
         Log.Error ("Cannot dispatch event type {0}", Event_Index'Image (Event.Kind));
         raise Not_Found;
      end if;

      declare
         Pos   : Queue_Dispatcher_Lists.Cursor := Manager.Actions (Event.Kind).Queues.First;
      begin
         --  Find the queue.
         while Queue_Dispatcher_Lists.Has_Element (Pos) loop
            Queue_Dispatcher_Lists.Query_Element (Pos, Find_Queue'Access);
            exit when Found;
            Queue_Dispatcher_Lists.Next (Pos);
         end loop;
      end;
   end Dispatch;

   --  ------------------------------
   --  Find the event queue identified by the given name.
   --  ------------------------------
   function Find_Queue (Manager : in Event_Manager;
                        Name    : in String) return AWA.Events.Queues.Queue_Access is
      Pos : constant Queue_Map.Cursor := Manager.Queues.Find (Name);
   begin
      if Queue_Map.Has_Element (Pos) then
         return Queue_Map.Element (Pos);
      else
         Log.Error ("Event queue {0} not found", Name);
         return null;
      end if;
   end Find_Queue;

   --  ------------------------------
   --  Add the event queue in the registry.
   --  ------------------------------
   procedure Add_Queue (Manager : in out Event_Manager;
                        Queue   : in AWA.Events.Queues.Queue_Access) is
   begin
      Manager.Queues.Insert (Key      => Queue.Get_Name,
                             New_Item => Queue);
   end Add_Queue;

   --  ------------------------------
   --  Add an action invoked when the event identified by <b>Event</b> is sent.
   --  The event is posted on the queue identified by <b>Queue</b>.
   --  When the event queue dispatches the event, the Ada bean identified by the method action
   --  represented by <b>Action</b> is created and initialized by evaluating and setting the
   --  parameters defined in <b>Params</b>.  The action method is then invoked.
   --  ------------------------------
   procedure Add_Action (Manager : in out Event_Manager;
                         Event   : in String;
                         Queue   : in AWA.Events.Queues.Queue_Access;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector) is

      procedure Add_Action (List : in out Queue_Dispatcher) is
         use type AWA.Events.Dispatchers.Dispatcher_Access;
      begin
         if List.Dispatcher = null then
            List.Dispatcher := new AWA.Events.Dispatchers.Actions.Action_Dispatcher;
         end if;
         List.Dispatcher.Add_Action (Action, Params);
      end Add_Action;

      Found : Boolean := False;

      procedure Find_Queue (List : in Queue_Dispatcher) is
      begin
         Found := List.Queue = Queue;
      end Find_Queue;

      Index : constant Event_Index := Find_Event_Index (Event);
      Pos   : Queue_Dispatcher_Lists.Cursor := Manager.Actions (Index).Queues.First;
   begin
      --  Find the queue.
      while Queue_Dispatcher_Lists.Has_Element (Pos) loop
         Queue_Dispatcher_Lists.Query_Element (Pos, Find_Queue'Access);
         exit when Found;
         Queue_Dispatcher_Lists.Next (Pos);
      end loop;

      --  Create it if it does not exist.
      if not Found then
         declare
            New_Queue : Queue_Dispatcher;
         begin
            New_Queue.Queue := Queue;
            Manager.Actions (Index).Queues.Append (New_Queue);
            Pos := Manager.Actions (Index).Queues.Last;
         end;
      end if;

      --  And append the new action to the event queue.
      Manager.Actions (Index).Queues.Update_Element (Pos, Add_Action'Access);
   end Add_Action;

   --  ------------------------------
   --  Initialize the event manager.
   --  ------------------------------
   procedure Initialize (Manager : in out Event_Manager;
                         DB      : in out ADO.Sessions.Master_Session) is
      Msg_Types : AWA.Queues.Models.Message_Type_Vector;

      Query     : ADO.SQL.Query;

      procedure Set_Events (Msg : in AWA.Queues.Models.Message_Type_Ref) is
         Name : constant String := Msg.Get_Name;
      begin
         declare
            Index : constant Event_Index := Find_Event_Index (Name);
         begin
            Manager.Actions (Index).Event := Msg;
         end;

      exception
         when others =>
            Log.Warn ("Event {0} is no longer used", Name);
      end Set_Events;

   begin
      Log.Info ("Initializing {0} events", Event_Index'Image (Last_Event));

      DB.Begin_Transaction;
      Manager.Actions := new Event_Queues_Array (1 .. Last_Event);

      AWA.Queues.Models.List (Object  => Msg_Types,
                              Session => DB,
                              Query   => Query);
      declare
         Pos : AWA.Queues.Models.Message_Type_Vectors.Cursor := Msg_Types.First;
      begin
         while AWA.Queues.Models.Message_Type_Vectors.Has_Element (Pos) loop
            AWA.Queues.Models.Message_Type_Vectors.Query_Element (Pos, Set_Events'Access);
            AWA.Queues.Models.Message_Type_Vectors.Next (Pos);
         end loop;
      end;

      for I in Manager.Actions'Range loop
         if Manager.Actions (I).Event.Is_Null then
            declare
               Name : constant Util.Strings.Name_Access := Get_Event_Type_Name (I);
            begin
               Log.Info ("Creating event type {0} in database", Name.all);
               Manager.Actions (I).Event.Set_Name (Name.all);
               Manager.Actions (I).Event.Save (DB);
            end;
         end if;
      end loop;
      DB.Commit;
   end Initialize;

   --  ------------------------------
   --  Finalize the event manager by releasing the allocated storage.
   --  ------------------------------
   overriding
   procedure Finalize (Manager : in out Event_Manager) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Event_Queues_Array,
                                        Name   => Event_Queues_Array_Access);
   begin
      Free (Manager.Actions);
   end Finalize;

end AWA.Events.Services;
