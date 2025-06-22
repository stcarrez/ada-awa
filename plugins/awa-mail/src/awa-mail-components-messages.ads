-----------------------------------------------------------------------
--  awa-mail-components-recipients -- Mail UI Recipients
--  Copyright (C) 2012, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Contexts.Faces;

--  === Mail Messages ===
--  The `AWA.Mail.Components.Messages` package defines the UI components
--  to represent the email message with its recipients, subject and body.
--
--  The mail message is retrieved by looking at the parent UI component until a
--  `UIMailMessage` component is found.  The mail message recipients are initialized
--  during the render response JSF phase, that is when `Encode_End` are called.
--
--  The `<mail:body>` component holds the message body.  This component can
--  include a facelet labeled `alternative` in which case it will be used
--  to build the `text/plain` mail message.  The default content type for
--  `<mail:body>` is `text/html` but this can be changed by using the
--  `type` attribute.
--
--    <mail:body type='text/html'>
--       <facet name='alternative'>
--          The text/plain mail message.
--       </facet>
--       The text/html mail message.
--    </mail:body>
package AWA.Mail.Components.Messages is

   ALTERNATIVE_NAME : constant String := "alternative";

   --  ------------------------------
   --  The mail recipient
   --  ------------------------------
   type UIMailMessage is new UIMailComponent with private;
   type UIMailMessage_Access is access all UIMailMessage'Class;

   --  Set the mail message instance.
   procedure Set_Message (UI      : in out UIMailMessage;
                          Message : in AWA.Mail.Clients.Mail_Message_Access);

   --  Get the mail message instance.
   overriding
   function Get_Message (UI      : in UIMailMessage) return AWA.Mail.Clients.Mail_Message_Access;

   --  Send the mail.
   overriding
   procedure Encode_End (UI      : in UIMailMessage;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Finalize and release the mail message.
   overriding
   procedure Finalize (UI : in out UIMailMessage);

   --  ------------------------------
   --  The mail subject
   --  ------------------------------
   type UIMailSubject is new UIMailComponent with private;

   --  Render the mail subject and initializes the message with its content.
   overriding
   procedure Encode_Children (UI      : in UIMailSubject;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  The mail body
   --  ------------------------------
   type UIMailBody is new UIMailComponent with private;

   --  Render the mail body and initializes the message with its content.
   overriding
   procedure Encode_Children (UI      : in UIMailBody;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type UIMailMessage is new UIMailComponent with record
      Message : AWA.Mail.Clients.Mail_Message_Access;
   end record;

   type UIMailBody is new UIMailComponent with null record;
   type UIMailSubject is new UIMailComponent with null record;

end AWA.Mail.Components.Messages;
