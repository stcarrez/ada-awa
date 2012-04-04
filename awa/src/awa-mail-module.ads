-----------------------------------------------------------------------
--  awa-mail -- Mail module
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with Util.Beans.Objects.Maps;

with ASF.Applications;

with AWA.Modules;
with AWA.Events;
with AWA.Mail.Clients;
package AWA.Mail.Module is

   NAME : constant String := "mail";

   type Mail_Module is new AWA.Modules.Module with private;
   type Mail_Module_Access is access all Mail_Module'Class;

   --  Initialize the mail module.
   overriding
   procedure Initialize (Plugin : in out Mail_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Create a new mail message.
   function Create_Message (Plugin : in Mail_Module)
                            return AWA.Mail.Clients.Mail_Message_Access;

   --  Get the mail template that must be used for the given event name.
   --  The mail template is configured by the property: <i>module</i>.template.<i>event</i>.
   function Get_Template (Plugin : in Mail_Module;
                          Name   : in String) return String;

   --  Format and send an email.
   procedure Send_Mail (Plugin   : in Mail_Module;
                        Template : in String;
                        Props    : in Util.Beans.Objects.Maps.Map;
                        Content  : in AWA.Events.Module_Event'Class);

   --  Get the mail module instance associated with the current application.
   function Get_Mail_Module return Mail_Module_Access;

private

   type Mail_Module is new AWA.Modules.Module with record
      Mailer : AWA.Mail.Clients.Mail_Manager_Access;
   end record;

end AWA.Mail.Module;
