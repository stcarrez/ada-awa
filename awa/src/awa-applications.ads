-----------------------------------------------------------------------
--  awa-applications -- Ada Web Application
--  Copyright (C) 2009 - 2020 Stephane Carrez
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
with Util.Serialize.IO;

with ASF.Applications.Main;
with ASF.Applications.Main.Configs;
with ADO.Sessions.Factory;
with AWA.Modules;
with AWA.Events;
with AWA.Events.Services;
with AWA.Audits.Services;

--  == Initialization ==
--  The AWA application is represented by the `Application` type which should
--  be extended for the final application to provide the modules and specific
--  components of the final application.
--
--  The initialization of an AWA application is made in several steps
--  represented by different procedures of the main `Application` type.
--  The whole initialization is handled by the `Initialize` procedure
--  which gets a first set of configuration properties and a factory
--  to build specific component.
--
--  The `Initialize` procedure will perform the following steps:
--
--  * It uses the factory to allocate the ASF lifecycle handler, the navigation
--    handler, the security manager, the OAuth manager, the exception
--    handlers.
--  * It calls the `Initialize_Components` procedure to let the
--    application register all the ASF components.  These components must
--    be registered before any configuration file is read.
--  * It calls the `Initialize_Config`
--  * It calls the `Initialize_Servlets` procedure to allow the application
--    to register all the servlet instances used by the application.
--  * It calls the `Initialize_Filters` procedure to allow the application
--    to register all the servlet filter instances.  The application servlets
--    and filters must be registered before reading the global configuration file.
--  * It loads the global application configuration by reading the `awa.xml`
--    file.  By reading this configuration, some global configuration is
--    established on the servlets, filters.
--  * It calls the `Initialize_Modules` procedure so that all the application
--    modules can be registered, configured and initialized.  Each module
--    brings its own component, servlet and filter.  They are configured
--    by their own XML configuration file.
--  * It loads the module application configuration by reading the XML
--    files described by the `app.config.plugins` configuration.  This last
--    step allows the application to setup and update the configuration
--    of all modules that have been registered.
--
--  == Configuration ==
--  The following global configuration parameter are defined:
--
--  @include-config awa.xml
--
package AWA.Applications is

   --  Directories where the configuration files are searched.
   package P_Module_Dir is
     new ASF.Applications.Main.Configs.Parameter ("app.modules.dir",
                                                  "#{fn:composePath(app_search_dirs,'config')}");

   --  A list of configuration files separated by ';'.  These files are
   --  searched in 'app.modules.dir' and loaded in the order specified before
   --  the application modules.
   package P_Config_File is
     new ASF.Applications.Main.Configs.Parameter ("app.config", "awa.xml");

   --  A list of configuration files separated by ';'.  These files are
   --  searched in 'app.modules.dir' and loaded in the order specified after
   --  all the application modules are initialized.
   package P_Plugin_Config_File is
     new ASF.Applications.Main.Configs.Parameter ("app.config.plugins", "");

   --  The database connection string to connect to the database.
   package P_Database is
     new ASF.Applications.Main.Configs.Parameter ("database",
                                                  "mysql://localhost:3306/db");

   --  The application contextPath configuration that gives the base URL
   --  of the application.
   package P_Context_Path is
     new ASF.Applications.Main.Configs.Parameter ("contextPath",
                                                  "");

   --  Module manager
   --
   --  The `Module_Manager` represents the root of the logic manager

   type Application is new ASF.Applications.Main.Application with private;
   type Application_Access is access all Application'Class;

   --  Initialize the application
   overriding
   procedure Initialize (App     : in out Application;
                         Conf    : in ASF.Applications.Config;
                         Factory : in out ASF.Applications.Main.Application_Factory'Class);

   --  Initialize the application configuration properties.
   --  Properties defined in `Conf` are expanded by using the EL
   --  expression resolver.
   overriding
   procedure Initialize_Config (App  : in out Application;
                                Conf : in out ASF.Applications.Config);

   --  Initialize the servlets provided by the application.
   --  This procedure is called by `Initialize`.
   --  It should register the application servlets.
   overriding
   procedure Initialize_Servlets (App : in out Application);

   --  Initialize the filters provided by the application.
   --  This procedure is called by `Initialize`.
   --  It should register the application filters.
   overriding
   procedure Initialize_Filters (App : in out Application);

   --  Initialize the ASF components provided by the application.
   --  This procedure is called by `Initialize`.
   --  It should register the component factories used by the application.
   overriding
   procedure Initialize_Components (App : in out Application);

   --  Read the application configuration file `awa.xml`.  This is called
   --  after the servlets and filters have been registered in the application
   --  but before the module registration.
   procedure Load_Configuration (App   : in out Application;
                                 Files : in String);

   --  Initialize the AWA modules provided by the application.
   --  This procedure is called by `Initialize`.
   --  It should register the modules used by the application.
   procedure Initialize_Modules (App : in out Application);

   --  Start the application.  This is called by the server container
   --  when the server is started.
   overriding
   procedure Start (App : in out Application);

   --  Close the application.
   overriding
   procedure Close (App : in out Application);

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

   --  Execute the `Process` procedure with the event manager used by the
   --  application.
   procedure Do_Event_Manager (App : in out Application;
                               Process : access procedure
                                 (Events : in out AWA.Events.Services.Event_Manager));

   --  Get the current application from the servlet context or service context.
   function Current return Application_Access;

private

   --  Initialize the parser represented by `Parser` to recognize the configuration
   --  that are specific to the plugins that have been registered so far.
   procedure Initialize_Parser (App : in out Application'Class;
                                Parser : in out Util.Serialize.IO.Parser'Class);

   type Application is new ASF.Applications.Main.Application with record
      DB_Factory : ADO.Sessions.Factory.Session_Factory;
      Modules    : aliased AWA.Modules.Module_Registry;
      Events     : aliased AWA.Events.Services.Event_Manager;
      Audits     : aliased AWA.Audits.Services.Audit_Manager;
   end record;

end AWA.Applications;
