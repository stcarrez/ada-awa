-----------------------------------------------------------------------
--  awa -- Ada Web Application
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with ASF.Applications.Main;
with ASF.Applications.Main.Configs;
with ADO.Sessions.Factory;
with AWA.Modules;
with AWA.Events;
with AWA.Events.Services;
package AWA.Applications is

   --  Directories where the configuration files are searched.
   package P_Module_Dir is
     new ASF.Applications.Main.Configs.Parameter ("app.modules.dir", "./config");

   --  A list of configuration files separated by ';'.  These files are searched in
   --  'app.modules.dir' and loaded in the order specified.
   package P_Config_File is
     new ASF.Applications.Main.Configs.Parameter ("app.config", "awa.xml");

   --  Module manager
   --
   --  The <b>Module_Manager</b> represents the root of the logic manager

   type Application is new ASF.Applications.Main.Application with private;
   type Application_Access is access all Application'Class;

   --  Initialize the application
   overriding
   procedure Initialize (App     : in out Application;
                         Conf    : in ASF.Applications.Config;
                         Factory : in out ASF.Applications.Main.Application_Factory'Class);

   --  Initialize the application configuration properties.  Properties defined in <b>Conf</b>
   --  are expanded by using the EL expression resolver.
   overriding
   procedure Initialize_Config (App  : in out Application;
                                Conf : in out ASF.Applications.Config);

   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   overriding
   procedure Initialize_Servlets (App : in out Application);

   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   overriding
   procedure Initialize_Filters (App : in out Application);

   --  Initialize the ASF components provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the component factories used by the application.
   overriding
   procedure Initialize_Components (App : in out Application);

   --  Read the application configuration file <b>awa.xml</b>.  This is called after the servlets
   --  and filters have been registered in the application but before the module registration.
   procedure Load_Configuration (App : in out Application);

   --  Initialize the AWA modules provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the modules used by the application.
   procedure Initialize_Modules (App : in out Application);

   --  Start the application.  This is called by the server container when the server is started.
   overriding
   procedure Start (App : in out Application);

   --  Register the module in the application
   procedure Register (App     : in Application_Access;
                       Module  : access AWA.Modules.Module'Class;
                       Name    : in String;
                       URI     : in String := "");

   --  Get the database connection for reading
   function Get_Session (App : Application)
                         return ADO.Sessions.Session;

   --  Get the database connection for writing
   function Get_Master_Session (App : Application)
                                return ADO.Sessions.Master_Session;

   --  Register the module in the application
   procedure Register (App     : in out Application;
                       Module  : in AWA.Modules.Module_Access;
                       Name    : in String;
                       URI     : in String := "");

   --  Find the module with the given name
   function Find_Module (App  : in Application;
                         Name : in String) return AWA.Modules.Module_Access;

   --  Send the event in the application event queues.
   procedure Send_Event (App   : in Application;
                         Event : in AWA.Events.Module_Event'Class);

   --  Execute the <tt>Process</tt> procedure with the event manager used by the application.
   procedure Do_Event_Manager (App : in out Application;
                               Process : access procedure
                                 (Events : in out AWA.Events.Services.Event_Manager));

private

   type Application is new ASF.Applications.Main.Application with record
      DB_Factory : ADO.Sessions.Factory.Session_Factory;
      Modules    : aliased AWA.Modules.Module_Registry;
      Events     : aliased AWA.Events.Services.Event_Manager;
   end record;

end AWA.Applications;
