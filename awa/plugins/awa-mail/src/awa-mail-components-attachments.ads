-----------------------------------------------------------------------
--  awa-mail-components-attachments -- Mail UI Attachments
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Contexts.Faces;

--  === Mail Attachments ===
--  The `AWA.Mail.Components.Attachments` package defines the UI components
--  to represent a mail attachment.  The mail attachment can be an external
--  file or may be provided by an Ada bean object.
--
package AWA.Mail.Components.Attachments is

   --  ------------------------------
   --  The mail recipient
   --  ------------------------------
   type UIMailAttachment is new UIMailComponent with private;
   type UIMailAttachment_Access is access all UIMailAttachment'Class;

   --  Render the mail subject and initializes the message with its content.
   overriding
   procedure Encode_Children (UI      : in UIMailAttachment;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type UIMailAttachment is new UIMailComponent with null record;

end AWA.Mail.Components.Attachments;
