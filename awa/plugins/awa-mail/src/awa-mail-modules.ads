-----------------------------------------------------------------------
--  awa-mail -- Mail module
--  Copyright (C) 2011, 2012, 2017, 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Objects.Maps;

with ASF.Applications;

with AWA.Modules;
with AWA.Events;
with AWA.Mail.Clients;

--  == Integration ==
--  To be able to use the `mail` module, you will need to add the following
--  line in your GNAT project file:
--
--    with "awa_mail";
--
--  The `Mail_Module` type represents the mail module.  An instance
--  of the mail module must be declared and registered when the application
--  is created and initialized.  The module instance can be defined
--  as follows:
--
--    type Application is new AWA.Applications.Application with record
--       Mail_Module : aliased AWA.Mail.Modules.Mail_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Mail.Modules.NAME,
--              Module => App.Mail_Module'Access);
--
--  == Configuration ==
--  The `mail` module needs some properties to configure the SMTP
--  server.
--
--  |Configuration    | Default   | Description                                     |
--  | --------------- | --------- | ----------------------------------------------- |
--  |mail.smtp.host   | localhost | Defines the SMTP server host name               |
--  |mail.smtp.port   | 25        | Defines the SMTP connection port                |
--  |mail.smtp.enable | 1         | Defines whether sending email is enabled or not |
--
--  == Sending an email ==
--  Sending an email when an event is posted can be done by using
--  an XML configuration.  Basically, the `mail` module uses the event
--  framework provided by AWA.  The XML definition looks like:
--
--    <on-event name="user-register">
--      <action>#{userMail.send}</action>
--      <property name="template">/mail/register-user-message.xhtml</property>
--    </on-event>
--
--  With this definition, the mail template `/mail/register-user-message.xhtml`
--  is formatted by using the event and application context when the
--  `user-register` event is posted.
--
--  @include awa-mail-components.ads
--  @include awa-mail-components-recipients.ads
--  @include awa-mail-components-messages.ads
--  @include awa-mail-components-attachments.ads
--
--  == Ada Beans ==
--  @include-bean mail.xml
--
package AWA.Mail.Modules is

   NAME : constant String := "mail";

   type Mail_Module is new AWA.Modules.Module with private;
   type Mail_Module_Access is access all Mail_Module'Class;

   --  Initialize the mail module.
   overriding
   procedure Initialize (Plugin : in out Mail_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Configures the module after its initialization and after having
   --  read its XML configuration.
   overriding
   procedure Configure (Plugin : in out Mail_Module;
                        Props  : in ASF.Applications.Config);

   --  Create a new mail message.
   function Create_Message (Plugin : in Mail_Module)
                            return AWA.Mail.Clients.Mail_Message_Access;

   --  Format and send an email.
   procedure Send_Mail (Plugin   : in Mail_Module;
                        Template : in String;
                        Props    : in Util.Beans.Objects.Maps.Map;
                        Params   : in Util.Beans.Objects.Maps.Map;
                        Content  : in AWA.Events.Module_Event'Class);

   --  Get the mail module instance associated with the current application.
   function Get_Mail_Module return Mail_Module_Access;

private

   type Mail_Module is new AWA.Modules.Module with record
      Mailer : AWA.Mail.Clients.Mail_Manager_Access;
   end record;

end AWA.Mail.Modules;
