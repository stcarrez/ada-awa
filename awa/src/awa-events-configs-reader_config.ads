-----------------------------------------------------------------------
--  awa-events-configs-reader_config -- Event configuration reader setup
--  Copyright (C) 2012, 2013, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Serialize.Mappers;
with EL.Contexts;

with AWA.Events.Services;

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
package AWA.Events.Configs.Reader_Config is

   Config : aliased Controller_Config;

   procedure Initialize;

end AWA.Events.Configs.Reader_Config;
