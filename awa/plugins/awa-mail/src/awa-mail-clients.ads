-----------------------------------------------------------------------
--  awa-mail-client -- Mail client interface
--  Copyright (C) 2012, 2020 Stephane Carrez
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
with Util.Properties;

--  The <b>AWA.Mail.Clients</b> package defines a mail client API used by the mail module.
--  It defines two interfaces that must be implemented.  This allows to have an implementation
--  based on an external application such as <tt>sendmail</tt> and other implementation that
--  use an SMTP connection.
package AWA.Mail.Clients is

   use Ada.Strings.Unbounded;

   type Recipient_Type is (TO, CC, BCC);

   --  ------------------------------
   --  Mail Message
   --  ------------------------------
   --  The <b>Mail_Message</b> represents an abstract mail message that can be initialized
   --  before being sent.
   type Mail_Message is limited interface;
   type Mail_Message_Access is access all Mail_Message'Class;

   --  Set the <tt>From</tt> part of the message.
   procedure Set_From (Message : in out Mail_Message;
                       Name    : in String;
                       Address : in String) is abstract;

   --  Add a recipient for the message.
   procedure Add_Recipient (Message : in out Mail_Message;
                            Kind    : in Recipient_Type;
                            Name    : in String;
                            Address : in String) is abstract;

   --  Set the subject of the message.
   procedure Set_Subject (Message : in out Mail_Message;
                          Subject : in String) is abstract;

   --  Set the body of the message.
   procedure Set_Body (Message      : in out Mail_Message;
                       Content      : in Unbounded_String;
                       Alternative  : in Unbounded_String;
                       Content_Type : in String := "") is abstract;

   --  Add an attachment with the given content.
   procedure Add_Attachment (Message      : in out Mail_Message;
                             Content      : in Unbounded_String;
                             Content_Id   : in String;
                             Content_Type : in String) is abstract;

   --  Add a file attachment.
   procedure Add_File_Attachment (Message      : in out Mail_Message;
                                  Filename     : in String;
                                  Content_Id   : in String;
                                  Content_Type : in String) is abstract;

   --  Send the email message.
   procedure Send (Message : in out Mail_Message) is abstract;

   --  ------------------------------
   --  Mail Manager
   --  ------------------------------
   --  The <b>Mail_Manager</b> is the entry point to create a new mail message
   --  and be able to send it.
   type Mail_Manager is limited interface;
   type Mail_Manager_Access is access all Mail_Manager'Class;

   --  Create a new mail message.
   function Create_Message (Manager : in Mail_Manager) return Mail_Message_Access is abstract;

   --  Factory to create the mail manager.  The mail manager implementation is identified by
   --  the <b>Name</b>.  It is configured according to the properties defined in <b>Props</b>.
   --  Returns null if the mail manager identified by the name is not supported.
   function Factory (Name  : in String;
                     Props : in Util.Properties.Manager'Class)
                     return Mail_Manager_Access;

end AWA.Mail.Clients;
