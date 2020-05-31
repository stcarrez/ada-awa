-----------------------------------------------------------------------
--  awa-mail-clients-files -- Mail client dump/file implementation
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
with Ada.Text_IO;
with Ada.IO_Exceptions;

with Util.Files;
with Util.Strings;
with Util.Log.Loggers;
package body AWA.Mail.Clients.Files is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Mail.Clients.Files");

   --  ------------------------------
   --  Set the <tt>From</tt> part of the message.
   --  ------------------------------
   overriding
   procedure Set_From (Message : in out File_Mail_Message;
                       Name    : in String;
                       Address : in String) is
   begin
      Message.From := To_Unbounded_String ("From: " & Name & " <" & Address & ">");
   end Set_From;

   --  ------------------------------
   --  Add a recipient for the message.
   --  ------------------------------
   overriding
   procedure Add_Recipient (Message : in out File_Mail_Message;
                            Kind    : in Recipient_Type;
                            Name    : in String;
                            Address : in String) is
      Email : constant String := Name & " <" & Address & ">";
   begin
      case Kind is
         when TO =>
            if Length (Message.To) = 0 then
               Append (Message.To, "To: ");
            else
               Append (Message.To, ", ");
            end if;
            Append (Message.To, Email);

         when CC =>
            if Length (Message.Cc) = 0 then
               Append (Message.Cc, "Cc: ");
            else
               Append (Message.Cc, ", ");
            end if;
            Append (Message.Cc, Email);

         when BCC =>
            if Length (Message.Bcc) = 0 then
               Append (Message.Bcc, "Bcc: ");
            else
               Append (Message.Bcc, ", ");
            end if;
            Append (Message.Bcc, Email);

      end case;
   end Add_Recipient;

   --  ------------------------------
   --  Set the subject of the message.
   --  ------------------------------
   overriding
   procedure Set_Subject (Message : in out File_Mail_Message;
                          Subject : in String) is
   begin
      Message.Subject := To_Unbounded_String (Subject);
   end Set_Subject;

   --  ------------------------------
   --  Set the body of the message.
   --  ------------------------------
   overriding
   procedure Set_Body (Message      : in out File_Mail_Message;
                       Content      : in Unbounded_String;
                       Alternative  : in Unbounded_String;
                       Content_Type : in String) is
   begin
      if Length (Alternative) > 0 then
         Append (Message.Message, "Content-Type: multipart/alternative; boundary=AAA"
                 & ASCII.LF & ASCII.LF);
         Append (Message.Message, "--AAA" & ASCII.LF);
      end if;
      Append (Message.Message, "Content-Type: ");
      Append (Message.Message, Content_Type);
      Append (Message.Message, ASCII.LF);
      Append (Message.Message, Content);
      if Length (Alternative) > 0 then
         Append (Message.Message, ASCII.LF & "--AAA" & ASCII.LF);
         Append (Message.Message, "Content-Type: text/plain; charset=utf-8" & ASCII.LF);
         Append (Message.Message, Alternative);
         Append (Message.Message, ASCII.LF & "--AAA" & ASCII.LF);
      end if;
   end Set_Body;

   --  ------------------------------
   --  Add an attachment with the given content.
   --  ------------------------------
   overriding
   procedure Add_Attachment (Message      : in out File_Mail_Message;
                             Content      : in Unbounded_String;
                             Content_Id   : in String;
                             Content_Type : in String) is
   begin
      Append (Message.Message, "Content-Type: ");
      Append (Message.Message, Content_Type);
      Append (Message.Message, "; boundary=BBB");
      Append (Message.Message, ASCII.LF);
      if Content_Id'Length > 0 then
         Append (Message.Message, "Content-Id: ");
         Append (Message.Message, Content_Id);
         Append (Message.Message, ASCII.LF);
      end if;
      Append (Message.Message, "==BBB" & ASCII.LF);
      Append (Message.Message, Content);
      Append (Message.Message, "==BBB" & ASCII.LF);
   end Add_Attachment;

   --  ------------------------------
   --  Add an attachment with the given content.
   --  ------------------------------
   overriding
   procedure Add_File_Attachment (Message      : in out File_Mail_Message;
                                  Filename     : in String;
                                  Content_Id   : in String;
                                  Content_Type : in String) is
   begin
      Append (Message.Message, "Content-Type: ");
      Append (Message.Message, Content_Type);
      Append (Message.Message, "; boundary=BBB");
      Append (Message.Message, ASCII.LF);
      if Content_Id'Length > 0 then
         Append (Message.Message, "Content-Id: ");
         Append (Message.Message, Content_Id);
         Append (Message.Message, ASCII.LF);
      end if;
      Append (Message.Message, "==BBB" & ASCII.LF);
      Append (Message.Message, Filename);
      Append (Message.Message, "==BBB" & ASCII.LF);
   end Add_File_Attachment;

   --  ------------------------------
   --  Send the email message.
   --  ------------------------------
   overriding
   procedure Send (Message : in out File_Mail_Message) is
      N : Natural;
      Output : Ada.Text_IO.File_Type;
   begin
      Util.Concurrent.Counters.Increment (Message.Manager.Index, N);
      declare
         Path : constant String := Util.Files.Compose (To_String (Message.Manager.Path),
                                                       "msg-" & Util.Strings.Image (N));
      begin
         Log.Info ("Sending dumb mail to {0}", Path);

         Ada.Text_IO.Create (File => Output,
                             Mode => Ada.Text_IO.Out_File,
                             Name => Path);

         Ada.Text_IO.Put_Line (Output, To_String (Message.From));
         if Length (Message.To) > 0 then
            Ada.Text_IO.Put_Line (Output, To_String (Message.To));
         end if;
         if Length (Message.Cc) > 0 then
            Ada.Text_IO.Put_Line (Output, To_String (Message.Cc));
         end if;
         if Length (Message.Bcc) > 0 then
            Ada.Text_IO.Put_Line (Output, To_String (Message.Bcc));
         end if;
         Ada.Text_IO.Put (Output, "Subject: ");
         Ada.Text_IO.Put_Line (Output, To_String (Message.Subject));
         Ada.Text_IO.Put_Line (Output, To_String (Message.Message));
         Ada.Text_IO.Put_Line (Output, To_String (Message.Attachments));
         Ada.Text_IO.Close (Output);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Cannot create mail file {0}", Path);
      end;

   end Send;

   --  ------------------------------
   --  Create a file based mail manager and configure it according to the properties.
   --  ------------------------------
   function Create_Manager (Props : in Util.Properties.Manager'Class) return Mail_Manager_Access is
      Result : constant File_Mail_Manager_Access := new File_Mail_Manager;
   begin
      Result.Self := Result;
      Result.Path := To_Unbounded_String (Props.Get (NAME & ".maildir", "mail"));
      return Result.all'Access;
   end Create_Manager;

   --  ------------------------------
   --  Create a new mail message.
   --  ------------------------------
   overriding
   function Create_Message (Manager : in File_Mail_Manager) return Mail_Message_Access is
      Result : constant File_Mail_Message_Access := new File_Mail_Message;
   begin
      Result.Manager := Manager.Self;
      return Result.all'Access;
   end Create_Message;

end AWA.Mail.Clients.Files;
