# Mail Module
The `mail` module allows an application to format and send a mail
to users.  This module does not define any web interface.  It provides
a set of services and methods to send a mail when an event is
received.  All this is done through configuration.  The module
defines a set of specific ASF components to format and prepare the
email.

## Integration
To be able to use the `mail` module, you will need to add the following
line in your GNAT project file:

```Ada
with "awa_mail";
```

The `Mail_Module` type represents the mail module.  An instance
of the mail module must be declared and registered when the application
is created and initialized.  The module instance can be defined
as follows:

```Ada
type Application is new AWA.Applications.Application with record
   Mail_Module : aliased AWA.Mail.Modules.Mail_Module;
end record;
```

And registered in the `Initialize_Modules` procedure by using:

```Ada
Register (App    => App.Self.all'Access,
          Name   => AWA.Mail.Modules.NAME,
          Module => App.Mail_Module'Access);
```

## Configuration
The `mail` module needs some properties to configure the SMTP
server.

|Configuration    | Default   | Description                                     |
| --------------- | --------- | ----------------------------------------------- |
|mail.smtp.host   | localhost | Defines the SMTP server host name               |
|mail.smtp.port   | 25        | Defines the SMTP connection port                |
|mail.smtp.enable | 1         | Defines whether sending email is enabled or not |

## Sending an email
Sending an email when an event is posted can be done by using
an XML configuration.  Basically, the `mail` module uses the event
framework provided by AWA.  The XML definition looks like:

```Ada
<on-event name="user-register">
  <action>#{userMail.send}</action>
  <property name="template">/mail/register-user-message.xhtml</property>
</on-event>
```

With this definition, the mail template `/mail/register-user-message.xhtml`
is formatted by using the event and application context when the
`user-register` event is posted.

## Components
The `AWA.Mail.Components` package defines several UI components that represent
a mail message in an ASF view.  The components allow the creation, formatting
and sending of a mail message using the same mechanism as the application
presentation layer.  Example:

```Ada
<f:view xmlns="mail:http://code.google.com/p/ada-awa/mail">
  <mail:message>
    <mail:subject>Welcome</mail:subject>
    <mail:to name="Iorek Byrnison">Iorek.Byrnison@svalbard.com</mail:to>
    <mail:body>
        ...
    </mail:body>
    <mail:attachment value="/images/mail-image.jpg"
       fileName="image.jpg"
       contentType="image/jpg"/>
  </mail:message>
</f:view>
```

When the view which contains these components is rendered, a mail message
is built and initialized by rendering the inner components.  The body and
other components can use other application UI components to render useful
content.  The email is send after the `mail:message` has finished to render
its inner children.

The `mail:subject` component describes the mail subject.

The `mail:to` component define the mail recipient.
There can be several recepients.

The `mail:body` component contains the mail body.

The `mail:attachment` component allows to include some attachment.

### Mail Recipients
The <b>AWA.Mail.Components.Recipients</b> package defines the UI components
to represent the <tt>To</tt>, <tt>From</tt>, <tt>Cc</tt> and <tt>Bcc</tt> recipients.

The mail message is retrieved by looking at the parent UI component until a
`UIMailMessage` component is found.  The mail message recipients are initialized
during the render response JSF phase, that is when <tt>Encode_End</tt> are called.

### Mail Messages
The `AWA.Mail.Components.Messages` package defines the UI components
to represent the email message with its recipients, subject and body.

The mail message is retrieved by looking at the parent UI component until a
`UIMailMessage` component is found.  The mail message recipients are initialized
during the render response JSF phase, that is when `Encode_End` are called.

The `<mail:body>` component holds the message body.  This component can
include a facelet labeled `alternative` in which case it will be used
to build the `text/plain` mail message.  The default content type for
`<mail:body>` is `text/html` but this can be changed by using the
`type` attribute.

```Ada
<mail:body type='text/html'>
   <facet name='alternative'>
      The text/plain mail message.
   </facet>
   The text/html mail message.
</mail:body>
```

### Mail Attachments
The `AWA.Mail.Components.Attachments` package defines the UI components
to represent a mail attachment.  The mail attachment can be an external
file or may be provided by an Ada bean object.



## Ada Beans

| Name           | Description                                                               |
|:---------------|:--------------------------------------------------------------------------|
|userMail|Bean used to send an email with a specific template to the user.|





