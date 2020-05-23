-----------------------------------------------------------------------
--  awa-events-dispatchers-actions -- Event dispatcher to Ada bean actions
--  Copyright (C) 2012, 2015, 2017, 2018, 2020 Stephane Carrez
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
with Ada.Exceptions;

with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Log.Loggers;

with EL.Contexts;
with EL.Contexts.TLS;
with EL.Variables;
with EL.Variables.Default;

with ASF.Beans;
with ASF.Requests;
with ASF.Sessions;

with AWA.Events.Action_Method;
package body AWA.Events.Dispatchers.Actions is

   use Ada.Strings.Unbounded;

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Events.Dispatchers.Actions");

   --  ------------------------------
   --  Dispatch the event identified by <b>Event</b>.
   --  The event actions which are associated with the event are executed synchronously.
   --  ------------------------------
   overriding
   procedure Dispatch (Manager : in Action_Dispatcher;
                       Event   : in Module_Event'Class) is

      use Util.Beans.Objects;

      --  Dispatch the event to the event action identified by <b>Action</b>.
      procedure Dispatch_One (Action : in Event_Action);

      type Event_Bean is new Util.Beans.Basic.Readonly_Bean with null record;

      overriding
      function Get_Value (From : in Event_Bean;
                          Name : in String) return Util.Beans.Objects.Object;

      --  ------------------------------
      --  Get the value identified by the name.
      --  If the name cannot be found, the method should return the Null object.
      --  ------------------------------
      overriding
      function Get_Value (From : in Event_Bean;
                          Name : in String) return Util.Beans.Objects.Object is
         pragma Unreferenced (From);
      begin
         return Event.Get_Value (Name);
      end Get_Value;

      Variables      : aliased EL.Variables.Default.Default_Variable_Mapper;

      --  ------------------------------
      --  Default Resolver
      --  ------------------------------
      type Event_ELResolver is limited new EL.Contexts.ELResolver with record
         Request     : ASF.Requests.Request_Access;
         Application : AWA.Applications.Application_Access;
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

      --  ------------------------------
      --  Get the value associated with a base object and a given property.
      --  ------------------------------
      overriding
      function Get_Value (Resolver : Event_ELResolver;
                          Context  : EL.Contexts.ELContext'Class;
                          Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                          Name     : Unbounded_String) return Util.Beans.Objects.Object is
         use Util.Beans.Basic;
         use type ASF.Requests.Request_Access;

         Result : Object;
         Bean   : Util.Beans.Basic.Readonly_Bean_Access;
         Scope  : ASF.Beans.Scope_Type;
         Key    : constant String := To_String (Name);
      begin
         if Base /= null then
            return Base.Get_Value (Key);
         end if;

         if Resolver.Request /= null then
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
         end if;

         Resolver.Application.Create (Name, Context, Bean, Scope);
         if Bean = null then
            return Resolver.Application.Get_Global (Name, Context);
         end if;
         Result := To_Object (Bean);
         if Resolver.Request /= null then
            Resolver.Request.Set_Attribute (Key, Result);
         else
            Variables.Bind (Key, Result);
         end if;
         return Result;
      end Get_Value;

      --  ------------------------------
      --  Set the value associated with a base object and a given property.
      --  ------------------------------
      overriding
      procedure Set_Value (Resolver : in out Event_ELResolver;
                           Context  : in EL.Contexts.ELContext'Class;
                           Base     : access Util.Beans.Basic.Bean'Class;
                           Name     : in Unbounded_String;
                           Value    : in Util.Beans.Objects.Object) is
         pragma Unreferenced (Context);
         use type ASF.Requests.Request_Access;

         Key : constant String := To_String (Name);
      begin
         if Base /= null then
            Base.Set_Value (Name => Key, Value => Value);
         elsif Resolver.Request /= null then
            Resolver.Request.Set_Attribute (Name => Key, Value => Value);
         else
            Variables.Bind (To_String (Name), Value);
         end if;
      end Set_Value;

      Local_Event    : aliased Event_Bean;
      Resolver       : aliased Event_ELResolver;
      ELContext      : aliased EL.Contexts.TLS.TLS_Context;

      --  ------------------------------
      --  Dispatch the event to the event action identified by <b>Action</b>.
      --  ------------------------------
      procedure Dispatch_One (Action : in Event_Action) is
         use Ada.Exceptions;

         Method : EL.Expressions.Method_Info;
         Bean   : access Util.Beans.Basic.Readonly_Bean'Class;
      begin
         Method := Action.Action.Get_Method_Info (Context => ELContext);

         Bean := Util.Beans.Objects.To_Bean (Method.Object);
         if Bean /= null and then Bean.all in Util.Beans.Basic.Bean'Class then
            --  If we have a prepare method and the bean provides a Set_Value method,
            --  call the preparation method to fill the bean with some values.
            EL.Beans.Initialize (Util.Beans.Basic.Bean'Class (Bean.all),
                                 Action.Properties,
                                 ELContext);
         end if;

         --  Execute the specified method on the bean and give it the event object.
         AWA.Events.Action_Method.Execute (Method => Method,
                                           Param  => Event);

         --  If an exception is raised by the action, do not propagate it:
         --   o We have to dispatch the event to other actions.
         --   o The event may be dispatched asynchronously and there is no handler
         --     that could handle such exception
      exception
         when E : others =>
            Log.Error ("Error when executing event action {0}: {1}: {2}",
                       Action.Action.Get_Expression, Exception_Name (E), Exception_Message (E));
      end Dispatch_One;

      Pos  : Event_Action_Lists.Cursor := Manager.Actions.First;
   begin
      Resolver.Application := Manager.Application;

      ELContext.Set_Resolver (Resolver'Unchecked_Access);
      ELContext.Set_Variable_Mapper (Variables'Unchecked_Access);

      Variables.Bind (Name  => "event",
                      Value => To_Object (Local_Event'Unchecked_Access, STATIC));

      while Event_Action_Lists.Has_Element (Pos) loop
         Event_Action_Lists.Query_Element (Pos, Dispatch_One'Access);
         Event_Action_Lists.Next (Pos);
      end loop;
   end Dispatch;

   --  ------------------------------
   --  Add an action invoked when an event is dispatched through this dispatcher.
   --  When the event queue dispatches the event, the Ada bean identified by the method action
   --  represented by <b>Action</b> is created and initialized by evaluating and setting the
   --  parameters defined in <b>Params</b>.  The action method is then invoked.
   --  ------------------------------
   overriding
   procedure Add_Action (Manager : in out Action_Dispatcher;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector) is

      Item : Event_Action;
   begin
      Item.Action     := Action;
      Item.Properties := Params;
      Manager.Actions.Append (Item);
   end Add_Action;

   --  ------------------------------
   --  Create a new dispatcher associated with the application.
   --  ------------------------------
   function Create_Dispatcher (Application : in AWA.Applications.Application_Access)
                               return Dispatcher_Access is
      Result : constant Dispatcher_Access := new Action_Dispatcher '(Dispatcher with
                                                                     Application => Application,
                                                                     others => <>);
   begin
      return Result.all'Access;
   end Create_Dispatcher;

end AWA.Events.Dispatchers.Actions;
