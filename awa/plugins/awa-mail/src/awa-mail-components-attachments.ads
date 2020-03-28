-----------------------------------------------------------------------
--  awa-mail-components-attachments -- Mail UI Attachments
--  Copyright (C) 2020 Stephane Carrez
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
