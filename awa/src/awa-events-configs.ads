-----------------------------------------------------------------------
--  awa-events-configs -- Event configuration
--  Copyright (C) 2012, 2013, 2017 Stephane Carrez
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
with Util.Serialize.Mappers;
with EL.Expressions;
with EL.Beans;
with EL.Contexts;
with ADO.Sessions;

with AWA.Events.Queues;
with AWA.Events.Services;
package AWA.Events.Configs is

   --  ------------------------------
   --  Event Config Controller
   --  ------------------------------
   type Controller_Config is record
      Name       : Util.Beans.Objects.Object;
      Queue      : AWA.Events.Queues.Queue_Ref;
      Queue_Type : Util.Beans.Objects.Object;
      Prop_Name  : Util.Beans.Objects.Object;
      Params     : EL.Beans.Param_Vectors.Vector;
      Priority   : Integer;
      Count      : Integer;
      Manager    : AWA.Events.Services.Event_Manager_Access;
      Action     : EL.Expressions.Method_Expression;
      Properties : EL.Beans.Param_Vectors.Vector;
      Context    : EL.Contexts.ELContext_Access;
      Session    : ADO.Sessions.Session;
      Match      : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Controller_Config_Access is access all Controller_Config;

   type Config_Fields is (FIELD_ON_EVENT, FIELD_NAME, FIELD_QUEUE_NAME, FIELD_ACTION,
                          FIELD_PROPERTY_NAME, FIELD_QUEUE, FIELD_TYPE,
                          FIELD_PROPERTY_VALUE,
                          FIELD_DISPATCHER, FIELD_DISPATCHER_QUEUE, FIELD_DISPATCHER_PRIORITY,
                          FIELD_DISPATCHER_COUNT);

   --  Set the configuration value identified by <b>Value</b> after having parsed
   --  the element identified by <b>Field</b>.
   procedure Set_Member (Into  : in out Controller_Config;
                         Field : in Config_Fields;
                         Value : in Util.Beans.Objects.Object);

   --  Setup the XML parser to read the <b>queue</b> and <b>on-event</b> description.
   --  For example:
   --
   --  <dispatcher name="async">
   --     <queue name="async"/>
   --     <queue name="persist"/>
   --     <count>4</count>
   --     <priority>10</priority>
   --  </dispatcher>
   --
   --  <queue name="async" type="fifo">
   --     <property name="size">254</property>
   --  </queue>
   --
   --  <queue name="defer" type="persist">
   --  </queue>
   --
   --  <on-event name="create-user" queue="async">
   --     <action>#{mail.send}</action>
   --     <property name="user">#{event.name}</property>
   --     <property name="template">mail/welcome.xhtml</property>
   --  </on-event>
   --
   --  This defines an event action called when the <b>create-user</b> event is posted.
   --  The Ada bean <b>mail</b> is created and is populated with the <b>user</b> and
   --  <b>template</b> properties.  The Ada bean action method <b>send</b> is called.
   generic
      Mapper  : in out Util.Serialize.Mappers.Processing;
      Manager : in AWA.Events.Services.Event_Manager_Access;
      Context : in EL.Contexts.ELContext_Access;
   package Reader_Config is
      Config : aliased Controller_Config;
   end Reader_Config;

end AWA.Events.Configs;
