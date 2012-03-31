-----------------------------------------------------------------------
--  awa-mail-components-messages -- Mail UI Message
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
with Ada.Unchecked_Deallocation;

with AWA.Mail.Clients;
package body AWA.Mail.Components.Messages is

   --  ------------------------------
   --  Set the mail message instance.
   --  ------------------------------
   procedure Set_Message (UI      : in out UIMailMessage;
                          Message : in AWA.Mail.Clients.Mail_Message_Access) is
   begin
      UI.Message := Message;
   end Set_Message;

   --  ------------------------------
   --  Get the mail message instance.
   --  ------------------------------
   overriding
   function Get_Message (UI : in UIMailMessage) return AWA.Mail.Clients.Mail_Message_Access is
   begin
      return UI.Message;
   end Get_Message;

   --  ------------------------------
   --  Render the end of the input component.  Closes the DL/DD list.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIMailMessage;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type AWA.Mail.Clients.Mail_Message_Access;
   begin
      if UI.Message /= null and UI.Is_Rendered (Context) then
         UI.Message.Send;
      end if;
   end Encode_End;

   --  ------------------------------
   --  Finalize and release the mail message.
   --  ------------------------------
   overriding
   procedure Finalize (UI : in out UIMailMessage) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => AWA.Mail.Clients.Mail_Message'Class,
                                         Name   => AWA.Mail.Clients.Mail_Message_Access);
   begin
      Free (UI.Message);
   end Finalize;

   --  ------------------------------
   --  Render the mail subject and initializes the message with its content.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIMailSubject;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      procedure Process (Content : in String);

      procedure Process (Content : in String) is
         Msg : constant AWA.Mail.Clients.Mail_Message_Access := UI.Get_Message;
      begin
         Msg.Set_Subject (Content);
      end Process;

   begin
      UI.Wrap_Encode_Children (Context, Process'Access);
   end Encode_Children;

   --  ------------------------------
   --  Render the mail body and initializes the message with its content.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIMailBody;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      procedure Process (Content : in String);

      procedure Process (Content : in String) is
         Msg : constant AWA.Mail.Clients.Mail_Message_Access := UI.Get_Message;
      begin
         Msg.Set_Body (Content);
      end Process;

   begin
      UI.Wrap_Encode_Children (Context, Process'Access);
   end Encode_Children;

end AWA.Mail.Components.Messages;
