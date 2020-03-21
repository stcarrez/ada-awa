-----------------------------------------------------------------------
--  awa-events -- AWA Events
--  Copyright (C) 2012, 2015, 2018, 2020 Stephane Carrez
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

with Util.Events;
with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Beans.Objects.Maps;

with ADO.Objects;
with ADO.Sessions;

with AWA.Index_Arrays;

--  == AWA Events ==
--  The `AWA.Events` package defines an event framework for modules to post
--  events and have Ada bean methods be invoked when these events are dispatched.
--  Subscription to events is done through configuration files.  This allows to
--  configure the modules and integrate them together easily at configuration time.
--
--  === Declaration ===
--  Modules define the events that they can generate by instantiating the
--  `Definition` package. This is a static definition of the event.  Each event
--  is given a unique name.
--
--    with AWA.Events.Definition;
--    ...
--    package Event_New_User is new AWA.Events.Definition ("new-user");
--
--  === Posting an event ===
--  The module can post an event to inform other modules or the system that
--  a particular action occurred.  The module creates the event instance of
--  type `Module_Event` and populates that event with useful properties for
--  event receivers.
--
--    with AWA.Events;
--    ...
--    Event : AWA.Events.Module_Event;
--
--    Event.Set_Event_Kind (Event_New_User.Kind);
--    Event.Set_Parameter ("email", "harry.potter@hogwarts.org");
--
--  The module will post the event by using the <b>Send_Event</b> operation.
--
--    Manager.Send_Event (Event);
--
--  === Receiving an event ===
--  Modules or applications interested by a particular event will configure
--  the event manager to dispatch the event to an Ada bean event action.
--  The Ada bean is an object that must implement a procedure that matches
--  the prototype:
--
--    type Action_Bean is new Util.Beans.Basic.Readonly_Bean ...;
--    procedure Action (Bean : in out Action_Bean;
--                      Event : in AWA.Events.Module_Event'Class);
--
--  The Ada bean method and object are registered as other Ada beans.
--
--  The configuration file indicates how to bind the Ada bean action and
--  the event together.  The action is specified using an EL Method Expression
--  (See Ada EL or JSR 245).
--
--    <on-event name="new_user">
--        <action>#{ada_bean.action}</action>
--    </on-event>
--
--  === Event queues and dispatchers ===
--  The `AWA.Events` framework posts events on queues and it uses a dispatcher
--  to process them.  There are two kinds of dispatchers:
--
--    * Synchronous dispatcher process the event when it is posted.
--      The task which posts the event invokes the Ada bean action.
--      In this dispatching mode, there is no event queue.
--      If the action method raises an exception, it will however be blocked.
--
--    * Asynchronous dispatcher are executed by dedicated tasks.  The event
--      is put in an event queue.  A dispatcher task processes the event and
--      invokes the action method at a later time.
--
--  When the event is queued, there are two types of event queues:
--
--    * A Fifo memory queue manages the event and dispatches them in FIFO order.
--      If the application is stopped, the events present in the Fifo queue
--      are lost.
--
--    * A persistent event queue manages the event in a similar way as the
--      FIFO queue but saves them in the database.  If the application is
--      stopped, events that have not yet been processed will be dispatched
--      when the application is started again.
--
--  === Data Model ===
--  [images/awa_events_model.png]
--
package AWA.Events is

   type Queue_Index is new Natural;
   type Event_Index is new Natural;

   package Event_Arrays is new AWA.Index_Arrays (Event_Index, String);

   use type Event_Arrays.Element_Type_Access;
   subtype Name_Access is Event_Arrays.Element_Type_Access;

   generic
   package Definition renames Event_Arrays.Definition;

   --  Exception raised if an event name is not found.
   Not_Found : exception renames Event_Arrays.Not_Found;

   --  Find the event runtime index given the event name.
   --  Raises Not_Found exception if the event name is not recognized.
   function Find_Event_Index (Name : in String)
                              return Event_Index renames Event_Arrays.Find;

   --  ------------------------------
   --  Module event
   --  ------------------------------
   type Module_Event is new Util.Events.Event
     and Util.Beans.Basic.Readonly_Bean with private;
   type Module_Event_Access is access all Module_Event'Class;

   --  Set the event type which identifies the event.
   procedure Set_Event_Kind (Event : in out Module_Event;
                             Kind  : in Event_Index);

   --  Get the event type which identifies the event.
   function Get_Event_Kind (Event : in Module_Event) return Event_Index;

   --  Set a parameter on the message.
   procedure Set_Parameter (Event  : in out Module_Event;
                            Name   : in String;
                            Value  : in String);

   --  Get the parameter with the given name.
   function Get_Parameter (Event : in Module_Event;
                           Name  : in String) return String;

   --  Set the parameters of the message.
   procedure Set_Parameters (Event      : in out Module_Event;
                             Parameters : in Util.Beans.Objects.Maps.Map);

   --  Get the value that corresponds to the parameter with the given name.
   overriding
   function Get_Value (Event : in Module_Event;
                       Name  : in String) return Util.Beans.Objects.Object;

   --  Get the entity identifier associated with the event.
   function Get_Entity_Identifier (Event : in Module_Event) return ADO.Identifier;

   --  Set the entity identifier associated with the event.
   procedure Set_Entity_Identifier (Event : in out Module_Event;
                                    Id    : in ADO.Identifier);

   --  Set the database entity associated with the event.
   procedure Set_Entity (Event   : in out Module_Event;
                         Entity  : in ADO.Objects.Object_Ref'Class;
                         Session : in ADO.Sessions.Session'Class);

   --  Copy the event properties to the map passed in `Into`.
   procedure Copy (Event : in Module_Event;
                   Into  : in out Util.Beans.Objects.Maps.Map);

private

   type Module_Event is new Util.Events.Event
     and Util.Beans.Basic.Readonly_Bean with record
      Kind        : Event_Index := Event_Arrays.Invalid_Index;
      Props       : Util.Beans.Objects.Maps.Map;
      Entity      : ADO.Identifier  := ADO.NO_IDENTIFIER;
      Entity_Type : ADO.Entity_Type := ADO.NO_ENTITY_TYPE;
   end record;

   --  The index of the last event definition.
   --  Last_Event : Event_Index := 0;

   --  Get the event type name.
   function Get_Event_Type_Name (Index : in Event_Index) return Name_Access
                                 renames Event_Arrays.Get_Element;

   --  Make and return a copy of the event.
   function Copy (Event : in Module_Event) return Module_Event_Access;

end AWA.Events;
