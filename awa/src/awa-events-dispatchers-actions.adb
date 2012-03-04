-----------------------------------------------------------------------
--  awa-events-dispatchers-actions -- Event dispatcher to Ada bean actions
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

with Util.Beans.Objects;
with Util.Beans.Basic;

with EL.Contexts;
with EL.Contexts.Default;
with EL.Variables;
with EL.Variables.Default;

with AWA.Applications;

with ASF.Beans;
with ASF.Requests;
with ASF.Sessions;

package body AWA.Events.Dispatchers.Actions is

   use Ada.Strings.Unbounded;

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

   --  ------------------------------
   --  Dispatch the event identified by <b>Event</b>.
   --  The event actions which are associated with the event are executed synchronously.
   --  ------------------------------
   procedure Dispatch (Manager : in Action_Dispatcher;
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

      Pos  : Event_Action_Lists.Cursor := Manager.Actions.First;
   begin
      --           Root_Resolver.Application := App'Unchecked_Access;
      --           Root_Resolver.Request := Request'Unchecked_Access;
      --           Root_Resolver.Beans := Beans'Unchecked_Access;
      ELContext.Set_Resolver (Resolver'Unchecked_Access);
      ELContext.Set_Variable_Mapper (Variables'Unchecked_Access);

      Variables.Bind (Name  => "event",
                      Value => To_Object (Local_Event'Unchecked_Access, STATIC));
      while not Event_Action_Lists.Has_Element (Pos) loop
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
   procedure Add_Action (Manager : in out Action_Dispatcher;
                         Action  : in EL.Expressions.Method_Expression;
                         Params  : in EL.Beans.Param_Vectors.Vector) is

      Item : Event_Action;
   begin
      Item.Action     := Action;
      Item.Properties := Params;
      Manager.Actions.Append (Item);
   end Add_Action;

end AWA.Events.Dispatchers.Actions;
