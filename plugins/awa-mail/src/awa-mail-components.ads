-----------------------------------------------------------------------
--  awa-mail-components -- Mail UI Components
--  Copyright (C) 2012, 2017, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Components.Core;

with AWA.Mail.Clients;

--  == Components ==
--  The `AWA.Mail.Components` package defines several UI components that represent
--  a mail message in an ASF view.  The components allow the creation, formatting
--  and sending of a mail message using the same mechanism as the application
--  presentation layer.  Example:
--
--    <f:view xmlns="mail:http://code.google.com/p/ada-awa/mail">
--      <mail:message>
--        <mail:subject>Welcome</mail:subject>
--        <mail:to name="Iorek Byrnison">Iorek.Byrnison@svalbard.com</mail:to>
--        <mail:body>
--            ...
--        </mail:body>
--        <mail:attachment value="/images/mail-image.jpg"
--           fileName="image.jpg"
--           contentType="image/jpg"/>
--      </mail:message>
--    </f:view>
--
--  When the view which contains these components is rendered, a mail message
--  is built and initialized by rendering the inner components.  The body and
--  other components can use other application UI components to render useful
--  content.  The email is send after the `mail:message` has finished to render
--  its inner children.
--
--  The `mail:subject` component describes the mail subject.
--
--  The `mail:to` component define the mail recipient.
--  There can be several recepients.
--
--  The `mail:body` component contains the mail body.
--
--  The `mail:attachment` component allows to include some attachment.
package AWA.Mail.Components is

   type UIMailComponent is new ASF.Components.Core.UIComponentBase with private;

   --  Get the mail message instance.
   function Get_Message (UI      : in UIMailComponent) return AWA.Mail.Clients.Mail_Message_Access;

private

   type UIMailComponent is new ASF.Components.Core.UIComponentBase with null record;

end AWA.Mail.Components;
