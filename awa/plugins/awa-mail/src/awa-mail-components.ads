-----------------------------------------------------------------------
--  awa-mail-components -- Mail UI Components
--  Copyright (C) 2012, 2017, 2020 Stephane Carrez
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
