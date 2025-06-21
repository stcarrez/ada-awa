-----------------------------------------------------------------------
--  awa-events-services -- AWA Event Manager
--  Copyright (C) 2012, 2015, 2016, 2018, 2019, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Util.Log.Loggers;

with ADO.SQL;
with ADO.Sessions;

with AWA.Events.Dispatchers.Tasks;
with AWA.Events.Dispatchers.Actions;
package body AWA.Events.Services is

   Log : constant Util.Log.Loggers.Logger
     := Util.Log.Loggers.Create ("AWA.Events.Services");

   --  ------------------------------
   --  Send the event to the modules that subscribed to it.
   --  The event is sent on each event queue.  Event queues will dispatch the event
   --  by invoking immediately or later on the `Dispatch`> operation.
   --  The synchronous or asynchronous reception of the event depends
   --  on the event queue.
   --  ------------------------------
   procedure Send (Manager : in Event_Manager;
                   Event   : in Module_Event'Class) is
      procedure Send_Queue (Queue : in Queue_Dispatcher);

      procedure Send_Queue (Queue : in Queue_Dispatcher) is
      begin
         if not Queue.Queue.Has_Queue then
            Queue.Dispatcher.Dispatch (Event);
         else
            Queue.Queue.Enqueue (Event);
         end if;
      end Send_Queue;

      Name : constant Name_Access := Get_Event_Type_Name (Event.Kind);
   begin
      if Name = null then
         Log.Error ("Cannot send event type {0}", Event_Index'Image (Event.Kind));
         raise Not_Found;
      end if;

      --  Find the event queues associated with the event.
      --  Post the event on each queue.  Some queue can dispatch the event
      --  immediately while some others may dispatched it asynchronously.
      declare
         Pos  : Queue_Dispatcher_Lists.Cursor
           := Manager.Actions (Event.Kind).Queues.First;
      begin
         if not Queue_Dispatcher_Lists.Has_Element (Pos) then
            Log.Debug ("Sending event {0} but there is no listener", Name.all);
         else
            Log.Debug ("Sending event {0}", Name.all);
            loop
               Queue_Dispatcher_Lists.Query_Element (Pos, Send_Queue'Access);
               Queue_Dispatcher_Lists.Next (Pos);
               exit when not Queue_Dispatcher_Lists.Has_Element (Pos);
            end loop;
         end if;
      end;
   end Send;

   --  ------------------------------
   --  Set the event message type which correspond to the `Kind` event index.
   --  ------------------------------
   procedure Set_Message_Type (Manager : in Event_Manager;
                               Event   : in out AWA.Events.Models.Message_Ref;
                               Kind    : in Event_Index) is
   begin
      if not Event_Arrays.Is_Valid (Kind) then
         Log.Error ("Cannot send event type {0}", Event_Index'Image (Kind));
         raise Not_Found;
      end if;
      Event.Set_Status (AWA.Events.Models.QUEUED);
      Event.Set_Message_Type (Manager.Actions (Kind).Event);
   end Set_Message_Type;

   --  ------------------------------
   --  Set the event queue associated with the event message.
   --  The event queue identified by `Name` is searched to find the
   --  `Queue_Ref` instance.
   --  ------------------------------
   procedure Set_Event_Queue (Manager : in Event_Manager;
                              Event   : in out AWA.Events.Models.Message_Ref;
                              Name    : in String) is
      Queue : constant AWA.Events.Queues.Queue_Ref := Manager.Find_Queue (Name);
   begin
      Event.Set_Queue (Queue.Get_Queue);
   end Set_Event_Queue;

   --  ------------------------------
   --  Dispatch the event identified by `Event` and associated with the event
   --  queue `Queue`.  The event actions which are associated with the event are
   --  executed synchronously.
   --  ------------------------------
   procedure Dispatch (Manager : in Event_Manager;
                       Queue   : in AWA.Events.Queues.Queue_Ref;
                       Event   : in Module_Event'Class) is
      procedure Find_Queue (List : in Queue_Dispatcher);

      Found : Boolean := False;

      procedure Find_Queue (List : in Queue_Dispatcher) is
      begin
         if List.Queue = Queue then
            List.Dispatcher.Dispatch (Event);
            Found := True;
         end if;
      end Find_Queue;

      Name : constant Name_Access := Get_Event_Type_Name (Event.Kind);
   begin
      if Name = null then
         Log.Error ("Cannot dispatch event type {0}", Event_Index'Image (Event.Kind));
         raise Not_Found;
      end if;

      declare
         Pos  : Queue_Dispatcher_Lists.Cursor
           := Manager.Actions (Event.Kind).Queues.First;
      begin
         if not Queue_Dispatcher_Lists.Has_Element (Pos) then
            Log.Debug ("Dispatching event {0} but there is no listener", Name.all);
         else
            Log.Debug ("Dispatching event {0}", Name.all);
            loop
               Queue_Dispatcher_Lists.Query_Element (Pos, Find_Queue'Access);
               exit when Found;
               Queue_Dispatcher_Lists.Next (Pos);
               if not Queue_Dispatcher_Lists.Has_Element (Pos) then
                  Log.Debug ("Dispatched event {0} but there was no listener", Name.all);
                  exit;
               end if;
            end loop;
         end if;
      end;
   end Dispatch;

   --  ------------------------------
   --  Find the event queue identified by the given name.
   --  ------------------------------
   function Find_Queue (Manager : in Event_Manager;
                        Name    : in String) return AWA.Events.Queues.Queue_Ref is
      Pos : constant Queues.Maps.Cursor := Manager.Queues.Find (Name);
   begin
      if Queues.Maps.Has_Element (Pos) then
         return Queues.Maps.Element (Pos);
      else
         Log.Error ("Event queue {0} not found", Name);
         return AWA.Events.Queues.Null_Queue;
      end if;
   end Find_Queue;

   --  ------------------------------
   --  Add the event queue in the registry.
   --  ------------------------------
   procedure Add_Queue (Manager : in out Event_Manager;
                        Queue   : in AWA.Events.Queues.Queue_Ref) is
      Name : constant String := Queue.Get_Name;
   begin
      if Manager.Queues.Contains (Name) then
         Log.Error ("Event queue {0} already defined");
      else
         Log.Info ("Adding event queue {0}", Name);
      end if;
      Manager.Queues.Include (Key      => Name,
                              New_Item => Queue);
   end Add_Queue;

   --  ------------------------------
   --  Add an action invoked when the event identified by `Event` is sent.
   --  The event is posted on the queue identified by `Queue`.
   --  When the event queue dispatches the event, the Ada bean identified
   --  by the method action represented by `Action` is created and initialized
   --  by evaluating and setting the parameters defined in `Params`.
   --  The action method is then invoked.
   --  ------------------------------
   procedure Add_Action (Manager : in out Event_Manager;
                         Event   : in String;
                         Queue   : in AWA.Events.Queues.Queue_Ref;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector) is
      procedure Find_Queue (List : in Queue_Dispatcher);
      procedure Add_Action (List : in out Queue_Dispatcher);

      procedure Add_Action (List : in out Queue_Dispatcher) is
         use type AWA.Events.Dispatchers.Dispatcher_Access;
      begin
         if List.Dispatcher = null then
            List.Dispatcher := AWA.Events.Dispatchers.Actions.Create_Dispatcher
              (Application => Manager.Application.all'Access);
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
      Log.Info ("Adding action {0} to event {1}", Action.Get_Expression, Event);

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
   --  Add a dispatcher to process the event queues matching the `Match` string.
   --  The dispatcher can create up to `Count` tasks running at the priority
   --  `Priority`.
   --  ------------------------------
   procedure Add_Dispatcher (Manager  : in out Event_Manager;
                             Match    : in String;
                             Count    : in Positive;
                             Priority : in Positive) is
      use type AWA.Events.Dispatchers.Dispatcher_Access;
   begin
      Log.Info ("Adding event dispatcher with {0} tasks prio {1} and "
                & "dispatching queues '{2}'",
                Positive'Image (Count), Positive'Image (Priority), Match);

      for I in Manager.Dispatchers'Range loop
         if Manager.Dispatchers (I) = null then
            Manager.Dispatchers (I) :=
              Dispatchers.Tasks.Create_Dispatcher (Manager'Unchecked_Access,
                                                   Match, Count, Priority);
            return;
         end if;
      end loop;
      Log.Error ("Implementation limit is reached.  Too many dispatcher.");
   end Add_Dispatcher;

   --  ------------------------------
   --  Initialize the event manager.
   --  ------------------------------
   procedure Initialize (Manager : in out Event_Manager;
                         App     : in Application_Access) is
      procedure Set_Events (Msg : in AWA.Events.Models.Message_Type_Ref);

      procedure Set_Events (Msg : in AWA.Events.Models.Message_Type_Ref) is
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

      Msg_Types : AWA.Events.Models.Message_Type_Vector;
      Query     : ADO.SQL.Query;
      DB        : ADO.Sessions.Master_Session := App.Get_Master_Session;
   begin
      Log.Info ("Initializing {0} events", Event_Index'Image (Event_Arrays.Get_Last));

      Manager.Application := App;
      DB.Begin_Transaction;
      Manager.Actions := new Event_Queues_Array (1 .. Event_Arrays.Get_Last);

      AWA.Events.Models.List (Object  => Msg_Types,
                              Session => DB,
                              Query   => Query);
      declare
         Pos : AWA.Events.Models.Message_Type_Vectors.Cursor := Msg_Types.First;
      begin
         while Models.Message_Type_Vectors.Has_Element (Pos) loop
            Models.Message_Type_Vectors.Query_Element (Pos, Set_Events'Access);
            Models.Message_Type_Vectors.Next (Pos);
         end loop;
      end;

      for I in Manager.Actions'Range loop
         if Manager.Actions (I).Event.Is_Null then
            declare
               Name : constant Name_Access := Get_Event_Type_Name (I);
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
   --  Start the event manager.  The dispatchers are configured to dispatch
   --  the event queues and tasks are started to process asynchronous events.
   --  ------------------------------
   procedure Start (Manager : in out Event_Manager) is
      use type AWA.Events.Dispatchers.Dispatcher_Access;

      --  Dispatch the event queues to the dispatcher according
      --  to the dispatcher configuration.
      procedure Associate_Dispatcher (Key   : in String;
                                      Queue : in out Events.Queues.Queue_Ref);

      procedure Associate_Dispatcher (Key   : in String;
                                      Queue : in out Events.Queues.Queue_Ref) is
         pragma Unreferenced (Key);

         Added : Boolean := False;
      begin
         for I in reverse Manager.Dispatchers'Range loop
            if Manager.Dispatchers (I) /= null then
               Manager.Dispatchers (I).Add_Queue (Queue, Added);
               exit when Added;
            end if;
         end loop;
      end Associate_Dispatcher;

      Iter : AWA.Events.Queues.Maps.Cursor := Manager.Queues.First;
   begin
      Log.Info ("Starting the event manager");

      while AWA.Events.Queues.Maps.Has_Element (Iter) loop
         Manager.Queues.Update_Element (Iter, Associate_Dispatcher'Access);
         AWA.Events.Queues.Maps.Next (Iter);
      end loop;

      --  Start the dispatchers.
      for I in Manager.Dispatchers'Range loop
         exit when Manager.Dispatchers (I) = null;
         Manager.Dispatchers (I).Start;
      end loop;
   end Start;

   --  ------------------------------
   --  Stop the event manager.
   --  ------------------------------
   procedure Stop (Manager : in out Event_Manager) is
      use type AWA.Events.Dispatchers.Dispatcher_Access;
   begin
      Log.Info ("Stopping the event manager");

      --  Stop the dispatchers.
      for I in Manager.Dispatchers'Range loop
         exit when Manager.Dispatchers (I) = null;
         Manager.Dispatchers (I).Stop;
      end loop;
   end Stop;

   --  ------------------------------
   --  Get the application associated with the event manager.
   --  ------------------------------
   function Get_Application (Manager : in Event_Manager) return Application_Access is
   begin
      return Manager.Application;
   end Get_Application;

   --  ------------------------------
   --  Finalize the queue dispatcher releasing the dispatcher memory.
   --  ------------------------------
   procedure Finalize (Object : in out Queue_Dispatcher) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Dispatchers.Dispatcher'Class,
                                         Name   => Dispatchers.Dispatcher_Access);
   begin
      Free (Object.Dispatcher);
   end Finalize;

   --  ------------------------------
   --  Finalize the event queues and the dispatchers.
   --  ------------------------------
   procedure Finalize (Object : in out Event_Queues) is
   begin
      loop
         declare
            Pos : constant Queue_Dispatcher_Lists.Cursor := Object.Queues.First;
         begin
            exit when not Queue_Dispatcher_Lists.Has_Element (Pos);

            Object.Queues.Update_Element (Position => Pos,
                                          Process  => Finalize'Access);
            Object.Queues.Delete_First;
         end;
      end loop;
   end Finalize;

   --  ------------------------------
   --  Finalize the event manager by releasing the allocated storage.
   --  ------------------------------
   overriding
   procedure Finalize (Manager : in out Event_Manager) is
      use type AWA.Events.Dispatchers.Dispatcher_Access;

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Event_Queues_Array,
                                        Name   => Event_Queues_Array_Access);
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Dispatchers.Dispatcher'Class,
                                         Name   => Dispatchers.Dispatcher_Access);
   begin
      --  Stop the dispatcher first.
      for I in Manager.Dispatchers'Range loop
         exit when Manager.Dispatchers (I) = null;
         Free (Manager.Dispatchers (I));
      end loop;
      if Manager.Actions /= null then
         for I in Manager.Actions'Range loop
            Finalize (Manager.Actions (I));
         end loop;
         Free (Manager.Actions);
      end if;
   end Finalize;

end AWA.Events.Services;
