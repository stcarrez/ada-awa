-----------------------------------------------------------------------
--  awa-mail -- Mail module
--  Copyright (C) 2011 Stephane Carrez
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
with Ada.Exceptions;

with ASF.Modules.Beans;
with ASF.Beans;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Log.Loggers;

with AWS.SMTP.Client;
package body AWA.Mail.Module is

   use Util.Log;

   Object : aliased Mail_Module;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Mail.Module");

   --  ------------------------------
   --  Get the mail module instance.
   --  ------------------------------
   function Instance return Mail_Module_Access is
   begin
      return Object'Access;
   end Instance;

   --  Get the SMTP server to send an email
   function Get_Smtp_Server (Plugin : in Mail_Module) return AWS.SMTP.Receiver is
      Server : constant String := Plugin.Get_Config (Name => "smtp.host", Default => "localhost");
      Port   : constant String := Plugin.Get_Config (Name => "smtp.port", Default => "25");
   begin
      return AWS.SMTP.Client.Initialize (Server_Name => Server,
                                         Port => Positive'Value (Port));
   end Get_Smtp_Server;

   --  ------------------------------
   --  Get the mail template that must be used for the given event name.
   --  The mail template is configured by the property: <i>module</i>.template.<i>event</i>.
   --  ------------------------------
   function Get_Template (Plugin : in Mail_Module;
                          Name   : in String) return String is
      Prop_Name : constant String := Plugin.Get_Name & ".template." & Name;
   begin
      return Plugin.Get_Config (Prop_Name);
   end Get_Template;

   --  Receive an event sent by another module with <b>Send_Event</b> method.
   procedure Receive_Event (Plugin  : in out Mail_Module;
                            Content : in ASF.Events.Modules.Module_Event'Class) is
      Name : constant String := Content.Get_Parameter ("name");
   begin
      Log.Info ("Receive event {0}", Name);

      declare
         File  : constant String := Plugin.Get_Template (Name);
         Req   : ASF.Requests.Mockup.Request;
         Reply : ASF.Responses.Mockup.Response;
         Ptr   : constant Util.Beans.Basic.Readonly_Bean_Access := Content'Unrestricted_Access;
         Bean  : constant Util.Beans.Objects.Object := Util.Beans.Objects.To_Object (Ptr);
      begin
         Log.Info ("With template '{0}'", File);

         Req.Set_Path_Info (File);
         Req.Set_Method ("GET");
         Req.Set_Attribute (Name => "event", Value => Bean);

         Plugin.Get_Application.Dispatch (Page     => File,
                                          Request  => Req,
                                          Response => Reply);

         declare
            Email   : constant String := Content.Get_Parameter ("email");
            Server  : constant AWS.SMTP.Receiver := Get_Smtp_Server (Plugin);
            Content : Ada.Strings.Unbounded.Unbounded_String;
            Result  : AWS.SMTP.Status;
         begin
            Reply.Read_Content (Content);
            Log.Info ("Mail to be sent: {0}", Content);

            AWS.SMTP.Client.Send (Server  => Server,
                                  From    => AWS.SMTP.E_Mail (Name    => "awa",
                                                              Address => "noreply@vacs.fr"),
                                  To      => AWS.SMTP.E_Mail (Name    => Email,
                                                              Address => "Stephane.Carrez@vacs.fr"),
                                  Subject => "Welcome to awa",
                                  Message => Ada.Strings.Unbounded.To_String (Content),
                                  Status  => Result);

         exception
            when E : others =>
               Log.Error ("Error when sending email with template {0}: {1}: {2}", File,
                          Ada.Exceptions.Exception_Name (E),
                          Ada.Exceptions.Exception_Message (E));
         end;
      end;

   end Receive_Event;

end AWA.Mail.Module;
