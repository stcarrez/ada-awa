-----------------------------------------------------------------------
--  awa-mail-clients-aws_smtp -- Mail client implementation on top of AWS SMTP client
--  Copyright (C) 2012, 2016, 2020 Stephane Carrez
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

with Ada.Strings.Unbounded;
with Ada.Finalization;

with Util.Properties;

with AWS.SMTP;
with AWS.SMTP.Authentication.Plain;
private with AWS.Attachments;

--  The <b>AWA.Mail.Clients.AWS_SMTP</b> package provides an implementation of the
--  mail client interfaces on top of AWS SMTP client API.
package AWA.Mail.Clients.AWS_SMTP is

   NAME : constant String := "smtp";

   --  ------------------------------
   --  Mail Message
   --  ------------------------------
   --  The <b>Mail_Message</b> represents an abstract mail message that can be initialized
   --  before being sent.
   type AWS_Mail_Message is new Ada.Finalization.Limited_Controlled and Mail_Message with private;
   type AWS_Mail_Message_Access is access all AWS_Mail_Message'Class;

   --  Set the <tt>From</tt> part of the message.
   overriding
   procedure Set_From (Message : in out AWS_Mail_Message;
                       Name    : in String;
                       Address : in String);

   --  Add a recipient for the message.
   overriding
   procedure Add_Recipient (Message : in out AWS_Mail_Message;
                            Kind    : in Recipient_Type;
                            Name    : in String;
                            Address : in String);

   --  Set the subject of the message.
   overriding
   procedure Set_Subject (Message : in out AWS_Mail_Message;
                          Subject : in String);

   --  Set the body of the message.
   overriding
   procedure Set_Body (Message      : in out AWS_Mail_Message;
                       Content      : in Unbounded_String;
                       Alternative  : in Unbounded_String;
                       Content_Type : in String);

   --  Add an attachment with the given content.
   overriding
   procedure Add_Attachment (Message      : in out AWS_Mail_Message;
                             Content      : in Unbounded_String;
                             Content_Id   : in String;
                             Content_Type : in String);

   --  Send the email message.
   overriding
   procedure Send (Message : in out AWS_Mail_Message);

   --  Deletes the mail message.
   overriding
   procedure Finalize (Message : in out AWS_Mail_Message);

   --  ------------------------------
   --  Mail Manager
   --  ------------------------------
   --  The <b>Mail_Manager</b> is the entry point to create a new mail message
   --  and be able to send it.
   type AWS_Mail_Manager is new Mail_Manager with private;
   type AWS_Mail_Manager_Access is access all AWS_Mail_Manager'Class;

   --  Create a SMTP based mail manager and configure it according to the properties.
   function Create_Manager (Props : in Util.Properties.Manager'Class) return Mail_Manager_Access;

   --  Create a new mail message.
   overriding
   function Create_Message (Manager : in AWS_Mail_Manager) return Mail_Message_Access;

private

   type Recipients_Access is access all AWS.SMTP.Recipients;

   type AWS_Mail_Message is new Ada.Finalization.Limited_Controlled and Mail_Message with record
      Manager     : AWS_Mail_Manager_Access;
      From        : AWS.SMTP.E_Mail_Data;
      Subject     : Ada.Strings.Unbounded.Unbounded_String;
      --  Content     : Ada.Strings.Unbounded.Unbounded_String;
      To          : Recipients_Access := null;
      Attachments : AWS.Attachments.List;
   end record;

   type AWS_Mail_Manager is new Mail_Manager with record
      Self    : AWS_Mail_Manager_Access;
      Server  : AWS.SMTP.Receiver;
      Creds   : aliased AWS.SMTP.Authentication.Plain.Credential;
      Port    : Positive := 25;
      Enable  : Boolean := True;
      Secure  : Boolean := False;
      Auth    : Boolean := False;
   end record;

   procedure Initialize (Client : in out AWS_Mail_Manager'Class;
                         Props  : in Util.Properties.Manager'Class);

end AWA.Mail.Clients.AWS_SMTP;
