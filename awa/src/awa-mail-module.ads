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

with AWA.Modules;
with ASF.Events.Modules;
package AWA.Mail.Module is

   type Mail_Module is new AWA.Modules.Module with null record;
   type Mail_Module_Access is access all Mail_Module'Class;

   --  Initialize the mail module.
   overriding
   procedure Initialize (Plugin : in out Mail_Module;
                         App    : in AWA.Modules.Application_Access);

   --  Get the mail template that must be used for the given event name.
   --  The mail template is configured by the property: <i>module</i>.template.<i>event</i>.
   function Get_Template (Plugin : in Mail_Module;
                          Name   : in String) return String;

   --  Receive an event sent by another module with <b>Send_Event</b> method.
   procedure Receive_Event (Plugin  : in out Mail_Module;
                            Content : in ASF.Events.Modules.Module_Event'Class);

end AWA.Mail.Module;
