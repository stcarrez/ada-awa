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

with Util.Strings;
with Util.Events;
with Util.Beans.Objects;
with Util.Beans.Basic;

with ASF.Applications;

--  The <b>AWA.Events</b> package defines an event framework for modules to post events
--  and have Ada bean methods be invoked when these events are dispatched.  Subscription to
--  events is done through configuration files.  This allows to configure the modules and
--  integrate them together easily at configuration time.
--
--  1/ Modules define the events they can generate by instantiating the <b>Definition</b> package.
--     This is a static definition of the event.
--
--    package Event_New_User is new AWA.Events.Definition ("new-user");
--
--  2/ The module can post an event to inform other modules or the system that a particular
--  action occurred.  The module creates the event instance of type <b>Module_Event</b> and
--  populates that event with useful properties for event receivers.
--
--      Event : AWA.Events.Module_Event;
--
--      Event.Set_Event_Kind (Event_New_User.Kind);
--      Event.Set_Parameter ("email", "harry.potter@hogwarts.org");
--
--  3/ The module will post the event by using the <b>Send_Event</b> operation.
--
--    Manager.Send_Event (Event);
--
--
package AWA.Events is

   type Queue_Index is new Natural;
   type Event_Index is new Natural;

   --  ------------------------------
   --  Event kind definition
   --  ------------------------------
   --  This package must be instantiated for each event that a module can post.
   generic
      Name : String;
   package Definition is
      function Kind return Event_Index;
      pragma Inline_Always (Kind);
   end Definition;

   --  Exception raised if an event name is not found.
   Not_Found : exception;

   --  Identifies an invalid event.
   Invalid_Event : constant Event_Index := 0;

   --  Find the event runtime index given the event name.
   --  Raises Not_Found exception if the event name is not recognized.
   function Find_Event_Index (Name : in String) return Event_Index;

   --  ------------------------------
   --  Module event
   --  ------------------------------
   type Module_Event is new Util.Events.Event and Util.Beans.Basic.Readonly_Bean with private;
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

   --  Get the value that corresponds to the parameter with the given name.
   overriding
   function Get_Value (Event : in Module_Event;
                       Name  : in String) return Util.Beans.Objects.Object;

private

   type Module_Event is new Util.Events.Event and Util.Beans.Basic.Readonly_Bean with record
      Kind  : Event_Index := Invalid_Event;
      Props : ASF.Applications.Config;
   end record;

   --  The index of the last event definition.
   Last_Event : Event_Index := 0;

   --  Get the event type name.
   function Get_Event_Type_Name (Index : in Event_Index) return Util.Strings.Name_Access;

   --  Make and return a copy of the event.
   function Copy (Event : in Module_Event) return Module_Event_Access;

end AWA.Events;
