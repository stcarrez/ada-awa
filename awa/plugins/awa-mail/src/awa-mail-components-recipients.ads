-----------------------------------------------------------------------
--  awa-mail-components-recipients -- Mail UI Recipients
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
with ASF.Contexts.Faces;

--  === Mail Recipients ===
--  The <b>AWA.Mail.Components.Recipients</b> package defines the UI components
--  to represent the <tt>To</tt>, <tt>From</tt>, <tt>Cc</tt> and <tt>Bcc</tt> recipients.
--
--  The mail message is retrieved by looking at the parent UI component until a
--  `UIMailMessage` component is found.  The mail message recipients are initialized
--  during the render response JSF phase, that is when <tt>Encode_End</tt> are called.
package AWA.Mail.Components.Recipients is

   --  ------------------------------
   --  The mail recipient
   --  ------------------------------
   type UIMailRecipient is new UIMailComponent with private;
   type UIMailRecipient_Access is access all UIMailRecipient'Class;

   --  Set the recipient type.
   procedure Set_Recipient (UI        : in out UIMailRecipient;
                            Recipient : in AWA.Mail.Clients.Recipient_Type);

   --  Get the mail name.
   function Get_Name (UI      : in UIMailRecipient;
                      Context : in ASF.Contexts.Faces.Faces_Context'Class) return String;

   --  Get the mail address.
   function Get_Address (UI      : in UIMailRecipient;
                         Context : in ASF.Contexts.Faces.Faces_Context'Class) return String;

   --  Render the children components to obtain an email address and set the TO/CC/BCC address
   --  of the message with the result.
   overriding
   procedure Encode_Children (UI      : in UIMailRecipient;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  The message sender
   --  ------------------------------
   type UISender is new UIMailRecipient with private;

   --  Render the children components to obtain an email address and set the From address
   --  of the message with the result.
   overriding
   procedure Encode_Children (UI      : in UISender;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type UIMailRecipient is new UIMailComponent with record
      Recipient : AWA.Mail.Clients.Recipient_Type := AWA.Mail.Clients.TO;
   end record;

   type UISender is new UIMailRecipient with null record;

end AWA.Mail.Components.Recipients;
