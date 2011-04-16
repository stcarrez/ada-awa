-----------------------------------------------------------------------
--  awa -- Ada Web Application
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

package body AWA.Modules is

   procedure Initialize (Manager : in out Module_Manager;
                         Module  : in Module_Access) is
   begin
      Manager.Module := Module;
   end Initialize;

   --  Module manager
   --
   --  ------------------------------
   --  Get the database connection for reading
   --  ------------------------------
   function Get_Session (Manager : Module_Manager)
                            return ADO.Sessions.Session is
   begin
      return Manager.Module.Get_Session;
   end Get_Session;

   --  ------------------------------
   --  Get the database connection for writing
   --  ------------------------------
   function Get_Master_Session (Manager : Module_Manager)
                                   return ADO.Sessions.Master_Session is
   begin
      return Manager.Module.Get_Master_Session;
   end Get_Master_Session;

   --  ------------------------------
   --  Send the event to the module.  The module identified by <b>To</b> is
   --  found and the event is posted on its event channel.
   --  ------------------------------
   procedure Send_Event (Manager : in Module_Manager;
                         To      : in String;
                         Content : in ASF.Events.Modules.Module_Event'Class) is
   begin
      Manager.Module.Send_Event (To, Content);
   end Send_Event;

   procedure Initialize (Plugin : in out Module;
                         App    : access ASF.Applications.Main.Application'Class) is
   begin
      ASF.Modules.Module (Plugin).Initialize (App);
      Plugin.Awa_App := AWA.Applications.Application'Class (App.all)'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   --  Get the database connection for reading
   --  ------------------------------
   function Get_Session (Manager : Module)
                            return ADO.Sessions.Session is
   begin
      return Manager.Awa_App.Get_Session;
   end Get_Session;

   --  ------------------------------
   --  Get the database connection for writing
   --  ------------------------------
   function Get_Master_Session (Manager : Module)
                                return ADO.Sessions.Master_Session is
   begin
      return Manager.Awa_App.Get_Master_Session;
   end Get_Master_Session;

end AWA.Modules;
