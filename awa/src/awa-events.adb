-----------------------------------------------------------------------
--  awa-events -- AWA Events
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

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Util.Log.Loggers;

with EL.Contexts;
with EL.Contexts.Default;
with EL.Variables;
with EL.Variables.Default;

with ASF.Beans;
with ASF.Requests;
with ASF.Sessions;

with ADO.SQL;

--  with AWA.Events.Action_Method;
with AWA.Applications;
package body AWA.Events is

   use Ada.Strings.Unbounded;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Events");

   type Event_Name_Pair is record
      Name  : Util.Strings.Name_Access := null;
      Index : Event_Index              := 0;
   end record;

   type Event_Name_Pair_Array is array (Event_Index range <>) of Event_Name_Pair;
   type Event_Name_Pair_Array_Access is access all Event_Name_Pair_Array;

   --  A static list of event names.  This array is created during the elaboration
   --  of event definitions.  It is sorted on event names.
   Events     : Event_Name_Pair_Array_Access;

   --  The index of the last event definition.
   Last_Event : Event_Index := 0;

   --  ------------------------------
   --  Register an event by its name and allocate a unique runtime event index.
   --  ------------------------------
   procedure Add_Event (Name  : in Util.Strings.Name_Access;
                        Index : out Event_Index) is
      Left    : Event_Index := 1;
      Right   : Event_Index := Last_Event;
   begin
      --  Check the storage and allocate it if necessary.
      if Events = null then
         Events := new Event_Name_Pair_Array (1 .. 10);
      elsif Events'Last = Last_Event then
         declare
            N : Event_Name_Pair_Array_Access := new Event_Name_Pair_Array (1 .. Last_Event + 10);
         begin
            N (Events'Range) := Events.all;
         end;
      end if;

      --  Find the event position within the sorted table.
      while Left <= Right loop
         declare
            Pos  : constant Event_Index := (Left + Right + 1) / 2;
            Item : constant Util.Strings.Name_Access := Events (Pos).Name;
         begin
            if Name.all = Item.all then
               Log.Error ("Event {0} is already registered.", Name.all);
               Index := Events (Pos).Index;
               return;
            elsif Name.all < Item.all then
               Right := Pos - 1;
            else
               Left := Pos + 1;
            end if;
         end;
      end loop;

      --  Insert the new event at the good position now.
      if Left = 0 then
         Left := Left + 1;
      end if;
      if Left <= Last_Event then
         Events (Left + 1 .. Last_Event + 1) := Events (Left .. Last_Event);

      end if;
      Last_Event := Last_Event + 1;
      Events (Left).Name := Name;
      Events (Left).Index := Last_Event;
      Index := Last_Event;
--        for I in 1 .. Last_Event loop
--           Log.Info ("{0} -> {1}", Event_Index'Image (I), Events (I).Name.all);
--        end loop;
      Log.Debug ("Event {0} index is {1}", Name.all, Event_Index'Image (Index));
   end Add_Event;

   --  ------------------------------
   --  Event kind definition
   --  ------------------------------
   package body Definition is
      Event : Event_Index;

      Event_Name : aliased constant String := Name;

      function Kind return Event_Index is
      begin
         return Event;
      end Kind;

   begin
      Add_Event (Event_Name'Access, Event);
   end Definition;

   --  ------------------------------
   --  Find the event runtime index given the event name.
   --  Raises Not_Found exception if the event name is not recognized.
   --  ------------------------------
   function Find_Event_Index (Name : in String) return Event_Index is
      Left    : Event_Index := 1;
      Right   : Event_Index := Last_Event;
   begin
      while Left <= Right loop
         declare
            Pos  : constant Event_Index := (Left + Right + 1) / 2;
            Item : constant Util.Strings.Name_Access := Events (Pos).Name;
         begin
            if Name = Item.all then
               return Events (Pos).Index;
            elsif Name < Item.all then
               Right := Pos - 1;
            else
               Left := Pos + 1;
            end if;
         end;
      end loop;
      Log.Error ("Event {0} not recognized.", Name);
      raise Not_Found with "Event " & Name & " not found";
   end Find_Event_Index;

   --  ------------------------------
   --  Set the event type which identifies the event.
   --  ------------------------------
   procedure Set_Event_Kind (Event : in out Module_Event;
                             Kind  : in Event_Index) is
   begin
      Event.Kind := Kind;
   end Set_Event_Kind;

   --  ------------------------------
   --  Get the event type which identifies the event.
   --  ------------------------------
   function Get_Event_Kind (Event : in Module_Event) return Event_Index is
   begin
      return Event.Kind;
   end Get_Event_Kind;

   --  ------------------------------
   --  Set a parameter on the message.
   --  ------------------------------
   procedure Set_Parameter (Event  : in out Module_Event;
                            Name   : in String;
                            Value  : in String) is
   begin
      Event.Props.Set (Name, Value);
   end Set_Parameter;

   --  ------------------------------
   --  Get the parameter with the given name.
   --  ------------------------------
   function Get_Parameter (Event : in Module_Event;
                           Name  : in String) return String is
   begin
      return Event.Props.Get (Name, "");
   end Get_Parameter;

   --  ------------------------------
   --  Get the value that corresponds to the parameter with the given name.
   --  ------------------------------
   function Get_Value (Event : in Module_Event;
                       Name  : in String) return Util.Beans.Objects.Object is
   begin
      if Event.Props.Exists (Name) then
         return Util.Beans.Objects.To_Object (Event.Get_Parameter (Name));
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;


   --  ------------------------------
   --  Event manager
   --  ------------------------------

   --  Send the event to the modules that subscribed to it.
   --  The event is sent on each event queue.  Event queues will dispatch the event
   --  by invoking immediately or later on the <b>Dispatch</b> operation.  The synchronous
   --  or asynchronous reception of the event depends on the event queue.
   procedure Send (Manager : in Event_Manager;
                   Event   : in Module_Event'Class) is

      procedure Send_Queue (Queue : in Queue_Actions) is
      begin
         if Queue.Queue.Is_Null then
            Manager.Dispatch (Queue => 0,
                              Event => Event);
         end if;
      end Send_Queue;

      Pos  : Queue_Actions_Lists.Cursor := Manager.Actions (Event.Kind).Queues.First;
   begin
      while not Queue_Actions_Lists.Has_Element (Pos) loop
         Queue_Actions_Lists.Query_Element (Pos, Send_Queue'Access);
         Queue_Actions_Lists.Next (Pos);
      end loop;
   end Send;

   --  ------------------------------
   --  Default Resolver
   --  ------------------------------
   type Event_ELResolver is new EL.Contexts.ELResolver with record
      Request     : ASF.Requests.Request_Access;
      Application : AWA.Applications.Application_Access;
--        Beans       : Bean_Vector_Access;
   end record;

   overriding
   function Get_Value (Resolver : Event_ELResolver;
                       Context  : EL.Contexts.ELContext'Class;
                       Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                       Name     : Unbounded_String) return Util.Beans.Objects.Object;
   overriding
   procedure Set_Value (Resolver : in out Event_ELResolver;
                        Context  : in EL.Contexts.ELContext'Class;
                        Base     : access Util.Beans.Basic.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in Util.Beans.Objects.Object);

   --  Get the value associated with a base object and a given property.
   overriding
   function Get_Value (Resolver : Event_ELResolver;
                       Context  : EL.Contexts.ELContext'Class;
                       Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                       Name     : Unbounded_String) return Util.Beans.Objects.Object is
      use Util.Beans.Objects;
      use Util.Beans.Basic;
      use EL.Variables;

      Result : Object;
      Bean   : Util.Beans.Basic.Readonly_Bean_Access;
      Scope  : ASF.Beans.Scope_Type;
      Key    : constant String := To_String (Name);
   begin
      if Base /= null then
         return Base.Get_Value (Key);
      end if;

      Result := Resolver.Request.Get_Attribute (Key);
      if not Util.Beans.Objects.Is_Null (Result) then
         return Result;
      end if;

      --  If there is a session, look if the attribute is defined there.
      declare
         Session : constant ASF.Sessions.Session := Resolver.Request.Get_Session;
      begin
         if Session.Is_Valid then
            Result := Session.Get_Attribute (Key);
            if not Util.Beans.Objects.Is_Null (Result) then
               return Result;
            end if;
         end if;
      end;
      Resolver.Application.Create (Name, Context, Bean, Scope);
      if Bean = null then
         return Resolver.Application.Get_Global (Name, Context);
         --           raise No_Variable
         --             with "Bean not found: '" & To_String (Name) & "'";
      end if;
--        Resolver.Beans.Append (Bean_Object '(Key'Length, Bean, Key));
      Result := To_Object (Bean);
      Resolver.Request.Set_Attribute (Key, Result);
      return Result;
   end Get_Value;

   --  Set the value associated with a base object and a given property.
   overriding
   procedure Set_Value (Resolver : in out Event_ELResolver;
                        Context  : in EL.Contexts.ELContext'Class;
                        Base     : access Util.Beans.Basic.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in Util.Beans.Objects.Object) is
      pragma Unreferenced (Context);

      Key : constant String := To_String (Name);
   begin
      if Base /= null then
         Base.Set_Value (Name => Key, Value => Value);
      else
         Resolver.Request.Set_Attribute (Name => Key, Value => Value);
      end if;
   end Set_Value;

   --  Dispatch the event identified by <b>Event</b> and associated with the event
   --  queue <b>Queue</b>.  The event actions which are associated with the event are
   --  executed synchronously.
   procedure Dispatch (Manager : in Event_Manager;
                       Queue   : in Queue_Index;
                       Event   : in Module_Event'Class) is

      use Util.Beans.Objects;

      type Event_Bean is new Util.Beans.Basic.Readonly_Bean with null record;

      --  Get the value identified by the name.
      --  If the name cannot be found, the method should return the Null object.
      overriding
      function Get_Value (From : in Event_Bean;
                          Name : in String) return Util.Beans.Objects.Object is
      begin
         return Event.Get_Value (Name);
      end Get_Value;

      Local_Event    : aliased Event_Bean;
      Resolver       : aliased Event_ELResolver;
      ELContext      : aliased EL.Contexts.Default.Default_Context;
      Variables      : aliased EL.Variables.Default.Default_Variable_Mapper;

      --  Dispatch the event to the event action identified by <b>Action</b>.
      procedure Dispatch_One (Action : in Event_Action) is
         Method : EL.Expressions.Method_Info;
      begin
         Method := Action.Action.Get_Method_Info (Context => ELContext);

         if Method.Object.all in Util.Beans.Basic.Bean'Class then
            --  If we have a prepare method and the bean provides a Set_Value method,
            --  call the preparation method to fill the bean with some values.
            EL.Beans.Initialize (Util.Beans.Basic.Bean'Class (Method.Object.all),
                                 Action.Properties,
                                 ELContext);
         end if;

         --  Execute the specified method on the bean and give it the event object.
--           AWA.Events.Action_Method.Execute (Method => Method,
--                                             Param  => Event);
      end Dispatch_One;

      procedure Dispatch_List (List : in Queue_Actions) is
         Pos : Event_Action_Lists.Cursor := List.Actions.First;
      begin
         while not Event_Action_Lists.Has_Element (Pos) loop
            Event_Action_Lists.Query_Element (Pos, Dispatch_One'Access);
            Event_Action_Lists.Next (Pos);
         end loop;
      end Dispatch_List;

      Pos  : Queue_Actions_Lists.Cursor := Manager.Actions (Event.Kind).Queues.First;
   begin
      --           Root_Resolver.Application := App'Unchecked_Access;
      --           Root_Resolver.Request := Request'Unchecked_Access;
      --           Root_Resolver.Beans := Beans'Unchecked_Access;
      ELContext.Set_Resolver (Resolver'Unchecked_Access);
      ELContext.Set_Variable_Mapper (Variables'Unchecked_Access);

      Variables.Bind (Name  => "event",
                      Value => To_Object (Local_Event'Unchecked_Access, STATIC));

      while not Queue_Actions_Lists.Has_Element (Pos) loop
         Queue_Actions_Lists.Query_Element (Pos, Dispatch_List'Access);
         Queue_Actions_Lists.Next (Pos);
      end loop;
   end Dispatch;

   --  ------------------------------
   --  Add an action invoked when the event identified by <b>Event</b> is sent.
   --  The event is posted on the queue identified by <b>Queue</b>.
   --  When the event queue dispatches the event, the Ada bean identified by the method action
   --  represented by <b>Action</b> is created and initialized by evaluating and setting the
   --  parameters defined in <b>Params</b>.  The action method is then invoked.
   --  ------------------------------
   procedure Add_Action (Manager : in out Event_Manager;
                         Event   : in String;
                         Queue   : in String;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector) is
      Index : constant Event_Index := Find_Event_Index (Event);

      procedure Add_Action (List : in out Queue_Actions) is
         Item : Event_Action;
      begin
         Item.Action     := Action;
         Item.Properties := Params;
         List.Actions.Append (Item);
      end Add_Action;

      Found : Boolean := False;

      procedure Find_Queue (List : in Queue_Actions) is
      begin
         Found := String '(List.Queue.Get_Name) = Queue;
      end Find_Queue;

      Pos  : Queue_Actions_Lists.Cursor := Manager.Actions (Index).Queues.First;

   begin
      --  Find the queue.
      while Queue_Actions_Lists.Has_Element (Pos) loop
         Queue_Actions_Lists.Query_Element (Pos, Find_Queue'Access);
         exit when Found;
         Queue_Actions_Lists.Next (Pos);
      end loop;

      --  Create it if it does not exist.
      if not Found then
         declare
            New_Queue : Queue_Actions;
         begin
            New_Queue.Queue.Set_Name (Queue);
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
      Manager.Actions := new Event_Actions_Array (1 .. Last_Event);

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
            Log.Info ("Creating event type {0} in database", Events (I).Name.all);
            Manager.Actions (I).Event.Set_Name (Events (I).Name.all);
            Manager.Actions (I).Event.Save (DB);
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
         new Ada.Unchecked_Deallocation (Object => Event_Actions_Array,
                                         Name   => Event_Actions_Array_Access);
   begin
      Free (Manager.Actions);
   end Finalize;

end AWA.Events;
