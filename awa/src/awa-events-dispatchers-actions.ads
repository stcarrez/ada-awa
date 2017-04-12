-----------------------------------------------------------------------
--  awa-events-dispatchers-actions -- Event dispatcher to Ada bean actions
--  Copyright (C) 2012, 2017 Stephane Carrez
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

with Ada.Containers.Doubly_Linked_Lists;

with EL.Expressions;
with EL.Beans;

with AWA.Events.Queues;
with AWA.Applications;

--  The <b>AWA.Events.Dispatchers.Actions</b> package implements an event dispatcher
--  which calls a set of Ada bean action methods that have been registered on the dispatcher
--  during configuration time.
package AWA.Events.Dispatchers.Actions is

   --  ------------------------------
   --  Event action dispatcher
   --  ------------------------------

   type Action_Dispatcher (Application : AWA.Applications.Application_Access) is
     new Dispatcher with private;

   --  Dispatch the event identified by <b>Event</b>.
   --  The event actions which are associated with the event are executed synchronously.
   overriding
   procedure Dispatch (Manager : in Action_Dispatcher;
                       Event   : in Module_Event'Class);

   --  Add an action invoked when an event is dispatched through this dispatcher.
   --  When the event queue dispatches the event, the Ada bean identified by the method action
   --  represented by <b>Action</b> is created and initialized by evaluating and setting the
   --  parameters defined in <b>Params</b>.  The action method is then invoked.
   overriding
   procedure Add_Action (Manager : in out Action_Dispatcher;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector);

   --  Create a new dispatcher associated with the application.
   function Create_Dispatcher (Application : in AWA.Applications.Application_Access)
                               return Dispatcher_Access;

private

   --  An event action records a method expression which identifies an Ada bean and a method
   --  that must be invoked when the event is dispatched.  The Ada bean instance is populated
   --  by evaluating and setting the set of properties before calling the action method.
   type Event_Action is record
      Action     : EL.Expressions.Method_Expression;
      Properties : EL.Beans.Param_Vectors.Vector;
   end record;

   --  A list of event actions.
   package Event_Action_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Event_Action);

   --  The dispatcher maintains a list of event actions to which the events are dispatched.
   type Action_Dispatcher (Application : AWA.Applications.Application_Access) is
     new Dispatcher with record
      Actions     : Event_Action_Lists.List;
      Queue       : AWA.Events.Queues.Queue_Ref;
   end record;

end AWA.Events.Dispatchers.Actions;
