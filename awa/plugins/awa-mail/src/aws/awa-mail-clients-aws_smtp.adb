-----------------------------------------------------------------------
--  awa-mail-clients-aws_smtp -- Mail client implementation on top of AWS SMTP client
--  Copyright (C) 2012, 2016, 2017, 2020 Stephane Carrez
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

with AWS.SMTP.Client;

with Util.Log.Loggers;

package body AWA.Mail.Clients.AWS_SMTP is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Mail.Clients.AWS_SMTP");

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => AWS.SMTP.Recipients,
                                      Name   => Recipients_Access);

   --  Get a printable representation of the email recipients.
   function Image (Recipients : in AWS.SMTP.Recipients) return String;

   --  ------------------------------
   --  Set the <tt>From</tt> part of the message.
   --  ------------------------------
   overriding
   procedure Set_From (Message : in out AWS_Mail_Message;
                       Name    : in String;
                       Address : in String) is
   begin
      Message.From := AWS.SMTP.E_Mail (Name    => Name,
                                       Address => Address);
   end Set_From;

   --  ------------------------------
   --  Add a recipient for the message.
   --  ------------------------------
   overriding
   procedure Add_Recipient (Message : in out AWS_Mail_Message;
                            Kind    : in Recipient_Type;
                            Name    : in String;
                            Address : in String) is
      pragma Unreferenced (Kind);
   begin
      if Message.To = null then
         Message.To := new AWS.SMTP.Recipients (1 .. 1);
      else
         declare
            To : constant Recipients_Access := new AWS.SMTP.Recipients (1 .. Message.To'Last + 1);
         begin
            To (Message.To'Range) := Message.To.all;
            Free (Message.To);
            Message.To := To;
         end;
      end if;
      Message.To (Message.To'Last) := AWS.SMTP.E_Mail (Name    => Name,
                                                       Address => Address);
   end Add_Recipient;

   --  ------------------------------
   --  Set the subject of the message.
   --  ------------------------------
   overriding
   procedure Set_Subject (Message : in out AWS_Mail_Message;
                          Subject : in String) is
   begin
      Message.Subject := To_Unbounded_String (Subject);
   end Set_Subject;

   --  ------------------------------
   --  Set the body of the message.
   --  ------------------------------
   overriding
   procedure Set_Body (Message      : in out AWS_Mail_Message;
                       Content      : in Unbounded_String;
                       Alternative  : in Unbounded_String;
                       Content_Type : in String) is
   begin
      if Length (Alternative) = 0 then
         AWS.Attachments.Add (Message.Attachments, "",
                              AWS.Attachments.Value (To_String (Content)));
      else
         declare
            Parts : AWS.Attachments.Alternatives;
         begin
            AWS.Attachments.Add (Parts,
                                 AWS.Attachments.Value (To_String (Content),
                                                        Content_Type => Content_Type));
            AWS.Attachments.Add (Parts,
                                 AWS.Attachments.Value (To_String (Alternative),
                                                        Content_Type => "text/plain"));
            AWS.Attachments.Add (Message.Attachments, Parts);
         end;
      end if;
   end Set_Body;

   --  ------------------------------
   --  Add an attachment with the given content.
   --  ------------------------------
   overriding
   procedure Add_Attachment (Message      : in out AWS_Mail_Message;
                             Content      : in Unbounded_String;
                             Content_Id   : in String;
                             Content_Type : in String) is
      Data : constant AWS.Attachments.Content
        := AWS.Attachments.Value (Data => To_String (Content),
                                  Content_Id => Content_Id,
                                  Content_Type => Content_Type);
   begin
      AWS.Attachments.Add (Attachments => Message.Attachments,
                           Name => Content_Id,
                           Data => Data);
   end Add_Attachment;

   --  ------------------------------
   --  Get a printable representation of the email recipients.
   --  ------------------------------
   function Image (Recipients : in AWS.SMTP.Recipients) return String is
      Result : Unbounded_String;
   begin
      for I in Recipients'Range loop
         Append (Result, AWS.SMTP.Image (Recipients (I)));
      end loop;
      return To_String (Result);
   end Image;

   --  ------------------------------
   --  Send the email message.
   --  ------------------------------
   overriding
   procedure Send (Message : in out AWS_Mail_Message) is
      Result   : AWS.SMTP.Status;
   begin
      if Message.To = null then
         return;
      end if;

      if Message.Manager.Enable then
         Log.Info ("Send email from {0} to {1}",
                   AWS.SMTP.Image (Message.From), Image (Message.To.all));

         AWS.SMTP.Client.Send (Server  => Message.Manager.Server,
                               From    => Message.From,
                               To      => Message.To.all,
                               Subject => To_String (Message.Subject),
                               Attachments => Message.Attachments,
                               Status  => Result);
         if not AWS.SMTP.Is_Ok (Result) then
            Log.Error ("Cannot send email: {0}",
                       AWS.SMTP.Status_Message (Result));
         end if;

      else
         Log.Info ("Disable send email from {0} to {1}",
                   AWS.SMTP.Image (Message.From), Image (Message.To.all));
      end if;
   end Send;

   --  ------------------------------
   --  Deletes the mail message.
   --  ------------------------------
   overriding
   procedure Finalize (Message : in out AWS_Mail_Message) is
   begin
      Log.Info ("Finalize mail message");
      Free (Message.To);
   end Finalize;

   procedure Initialize (Client : in out AWS_Mail_Manager'Class;
                         Props  : in Util.Properties.Manager'Class) is separate;

   --  ------------------------------
   --  Create a SMTP based mail manager and configure it according to the properties.
   --  ------------------------------
   function Create_Manager (Props : in Util.Properties.Manager'Class) return Mail_Manager_Access is
      Server : constant String := Props.Get (Name => "smtp.host", Default => "localhost");
      Port   : constant String := Props.Get (Name => "smtp.port", Default => "25");
      Enable : constant String := Props.Get (Name => "smtp.enable", Default => "1");
      Result : constant AWS_Mail_Manager_Access := new AWS_Mail_Manager;
   begin
      Log.Info ("Creating SMTP mail manager to server {0}:{1}", Server, Port);

      Result.Port   := Positive'Value (Port);
      Result.Enable := Enable = "1" or Enable = "yes" or Enable = "true";
      Result.Self   := Result;
      Initialize (Result.all, Props);
      return Result.all'Access;
   end Create_Manager;

   --  ------------------------------
   --  Create a new mail message.
   --  ------------------------------
   overriding
   function Create_Message (Manager : in AWS_Mail_Manager) return Mail_Message_Access is
      Result : constant AWS_Mail_Message_Access := new AWS_Mail_Message;
   begin
      Result.Manager := Manager.Self;
      return Result.all'Access;
   end Create_Message;

end AWA.Mail.Clients.AWS_SMTP;
