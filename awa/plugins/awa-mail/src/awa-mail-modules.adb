-----------------------------------------------------------------------
--  awa-mail -- Mail module
--  Copyright (C) 2011, 2012, 2015, 2017, 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Modules.Beans;
with AWA.Modules.Get;
with AWA.Mail.Beans;
with AWA.Mail.Components.Factory;
with AWA.Applications;

with Security;
with Servlet.Core;
with Servlet.Sessions;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Log.Loggers;

package body AWA.Mail.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Mail.Module");

   package Register is new AWA.Modules.Beans (Module => Mail_Module,
                                              Module_Access => Mail_Module_Access);

   type Mail_Principal is new Security.Principal with null record;

   --  Get the principal name.
   overriding
   function Get_Name (From : in Mail_Principal) return String;

   --  ------------------------------
   --  Get the principal name.
   --  ------------------------------
   overriding
   function Get_Name (From : in Mail_Principal) return String is
      pragma Unreferenced (From);
   begin
      return "AWA Mailer";
   end Get_Name;

   --  ------------------------------
   --  Initialize the mail module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Mail_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the mail module");

      --  Add the Mail UI components.
      App.Add_Components (AWA.Mail.Components.Factory.Definition);

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Mail.Beans.Mail_Bean",
                         Handler => AWA.Mail.Beans.Create_Mail_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Mail_Module;
                        Props  : in ASF.Applications.Config) is
      Mailer : constant String := Props.Get ("mailer", "smtp");
   begin
      Log.Info ("Mail plugin is using {0} mailer", Mailer);
      Plugin.Mailer := AWA.Mail.Clients.Factory (Mailer, Props);
   end Configure;

   --  ------------------------------
   --  Create a new mail message.
   --  ------------------------------
   function Create_Message (Plugin : in Mail_Module)
                            return AWA.Mail.Clients.Mail_Message_Access is
   begin
      return Plugin.Mailer.Create_Message;
   end Create_Message;

   --  ------------------------------
   --  Receive an event sent by another module with <b>Send_Event</b> method.
   --  Format and send an email.
   --  ------------------------------
   procedure Send_Mail (Plugin   : in Mail_Module;
                        Template : in String;
                        Props    : in Util.Beans.Objects.Maps.Map;
                        Params   : in Util.Beans.Objects.Maps.Map;
                        Content  : in AWA.Events.Module_Event'Class) is
      Name       : constant String := Content.Get_Parameter ("name");
      Locale     : constant String := Content.Get_Parameter ("locale");
      App     : constant AWA.Modules.Application_Access := Plugin.Get_Application;
   begin
      Log.Info ("Receive event {0} with template {1}", Name, Template);

      if Template = "" then
         Log.Debug ("No email template associated with event {0}", Name);
         return;
      end if;

      declare
         use Util.Beans.Objects;

         Req     : ASF.Requests.Mockup.Request;
         Reply   : ASF.Responses.Mockup.Response;
         Session : Servlet.Sessions.Session;
         Ptr     : constant Util.Beans.Basic.Readonly_Bean_Access := Content'Unrestricted_Access;
         Bean    : constant Object := To_Object (Ptr, STATIC);
         Dispatcher : constant Servlet.Core.Request_Dispatcher
           := App.Get_Request_Dispatcher (Template);
         Iter : Maps.Cursor := Params.First;
      begin
         App.Create_Session (Session);
         Session.Set_Principal (new Mail_Principal);

         Req.Set_Session (Session);
         Req.Set_Request_URI (Template);
         Req.Set_Method ("GET");
         Req.Set_Attribute (Name => "event", Value => Bean);
         Req.Set_Attributes (Props);
         if Locale'Length > 0 then
            Req.Set_Header ("Accept-Language", Locale);
         end if;

         --  Setup the request parameters.
         while Maps.Has_Element (Iter) loop
            Req.Set_Parameter (Maps.Key (Iter), To_String (Maps.Element (Iter)));
            Maps.Next (Iter);
         end loop;
         Servlet.Core.Forward (Dispatcher, Req, Reply);
         App.Delete_Session (Session);
      end;
   end Send_Mail;

   --  ------------------------------
   --  Get the mail module instance associated with the current application.
   --  ------------------------------
   function Get_Mail_Module return Mail_Module_Access is
      function Get is new AWA.Modules.Get (Mail_Module, Mail_Module_Access, NAME);
   begin
      return Get;
   end Get_Mail_Module;

end AWA.Mail.Modules;
