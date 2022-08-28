-----------------------------------------------------------------------
--  awa-applications -- Ada Web Application
--  Copyright (C) 2009 - 2022 Stephane Carrez
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

with Ada.IO_Exceptions;

with ADO.Sessions.Sources;
with ADO.Configs;

with EL.Contexts.Default;
with Util.Files;
with Util.Log.Loggers;

with Security.Policies;

with ASF.Server;
with ASF.Servlets;

with AWA.Services.Contexts;
with AWA.Components.Factory;
with AWA.Applications.Factory;
with AWA.Applications.Configs;
with AWA.Helpers.Selectors;
with AWA.Permissions.Services;
with AWA.Permissions.Beans;
package body AWA.Applications is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Applications");

   --  ------------------------------
   --  Initialize the application
   --  ------------------------------
   overriding
   procedure Initialize (App     : in out Application;
                         Conf    : in ASF.Applications.Config;
                         Factory : in out ASF.Applications.Main.Application_Factory'Class) is
      Ctx    : AWA.Services.Contexts.Service_Context;
   begin
      Log.Info ("Initializing application");

      Ctx.Set_Context (App'Unchecked_Access, null);
      AWA.Applications.Factory.Set_Application (Factory, App'Unchecked_Access);
      ASF.Applications.Main.Application (App).Initialize (Conf, Factory);

      --  Load the application configuration before any module.
      Application'Class (App).Load_Configuration (App.Get_Config (P_Config_File.P));

      --  Register the application modules and load their configuration.
      Application'Class (App).Initialize_Modules;

      --  Load the plugin application configuration after the module are configured.
      Application'Class (App).Load_Configuration (App.Get_Config (P_Plugin_Config_File.P));
   end Initialize;

   --  ------------------------------
   --  Initialize the application configuration properties.  Properties defined in <b>Conf</b>
   --  are expanded by using the EL expression resolver.
   --  ------------------------------
   overriding
   procedure Initialize_Config (App  : in out Application;
                                Conf : in out ASF.Applications.Config) is
      Connection : ADO.Sessions.Sources.Data_Source;
   begin
      Log.Info ("Initializing configuration");

      if Conf.Get ("app.modules.dir", "") = "" then
         Conf.Set ("app.modules.dir", "#{fn:composePath(app_search_dirs,'config')}");
      end if;
      if Conf.Get ("bundle.dir", "") = "" then
         Conf.Set ("bundle.dir", "#{fn:composePath(app_search_dirs,'bundles')}");
      end if;
      if Conf.Get ("ado.queries.paths", "") = "" then
         Conf.Set ("ado.queries.paths", "#{fn:composePath(app_search_dirs,'db')}");
      end if;
      ADO.Configs.Initialize (Conf);
      Log.Info ("Using application search dir: {0}", Conf.Get ("app_search_dirs", ""));

      ASF.Applications.Main.Application (App).Initialize_Config (Conf);
      Connection.Set_Connection (Conf.Get (P_Database.P));
      Connection.Set_Property ("ado.queries.paths", Conf.Get ("ado.queries.paths", ""));
      Connection.Set_Property ("ado.queries.load", Conf.Get ("ado.queries.load", ""));
      App.DB_Factory.Create (Connection);
      App.DB_Factory.Set_Audit_Manager (App.Audits'Unchecked_Access);
      App.Audits.Initialize (App'Unchecked_Access);
      App.Events.Initialize (App'Unchecked_Access);
      AWA.Modules.Initialize (App.Modules, Conf);

      --  Use a specific error message when the NO_PERMISSION exception is raised.
      App.Get_Exception_Handler.Set_Message ("AWA.PERMISSIONS.NO_PERMISSION",
                                             "layout.exception_no_permission");

      App.Register_Class ("AWA.Helpers.Selectors.Select_List_Bean",
                          AWA.Helpers.Selectors.Create_Select_List_Bean'Access);

      App.Register_Class ("AWA.Permissions.Beans.Permission_Bean",
                          AWA.Permissions.Beans.Create_Permission_Bean'Access);
   end Initialize_Config;

   --  ------------------------------
   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   --  ------------------------------
   overriding
   procedure Initialize_Servlets (App : in out Application) is
   begin
      Log.Info ("Initializing application servlets");

      ASF.Applications.Main.Application (App).Initialize_Servlets;
   end Initialize_Servlets;

   --  ------------------------------
   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   --  ------------------------------
   overriding
   procedure Initialize_Filters (App : in out Application) is
   begin
      Log.Info ("Initializing application filters");

      ASF.Applications.Main.Application (App).Initialize_Filters;
   end Initialize_Filters;

   --  ------------------------------
   --  Initialize the ASF components provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the component factories used by the application.
   --  ------------------------------
   overriding
   procedure Initialize_Components (App : in out Application) is
      procedure Register_Functions is
         new ASF.Applications.Main.Register_Functions (AWA.Permissions.Services.Set_Functions);
   begin
      Log.Info ("Initializing application components");

      ASF.Applications.Main.Application (App).Initialize_Components;
      App.Add_Components (AWA.Components.Factory.Definition);
      Register_Functions (App);
   end Initialize_Components;

   --  ------------------------------
   --  Read the application configuration file <b>awa.xml</b>.  This is called after the servlets
   --  and filters have been registered in the application but before the module registration.
   --  ------------------------------
   procedure Load_Configuration (App   : in out Application;
                                 Files : in String) is
      procedure Load_Config (File : in String;
                             Done : out Boolean);

      Paths     : constant String := App.Get_Config (P_Module_Dir.P);
      Ctx       : aliased EL.Contexts.Default.Default_Context;

      procedure Load_Config (File : in String;
                             Done : out Boolean) is
         Path  : constant String := Util.Files.Find_File_Path (File, Paths);
      begin
         Done := False;
         AWA.Applications.Configs.Read_Configuration (App, Path, Ctx'Unchecked_Access, True);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Warn ("Application configuration file '{0}' does not exist", Path);

      end Load_Config;

   begin
      Util.Files.Iterate_Path (Files, Load_Config'Access);
   end Load_Configuration;

   --  ------------------------------
   --  Initialize the AWA modules provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the modules used by the application.
   --  ------------------------------
   procedure Initialize_Modules (App : in out Application) is
   begin
      null;
   end Initialize_Modules;

   --  ------------------------------
   --  Start the application.  This is called by the server container when the server is started.
   --  ------------------------------
   overriding
   procedure Start (App : in out Application) is
      use type Security.Policies.Policy_Manager_Access;

      Manager : constant Security.Policies.Policy_Manager_Access
        := App.Get_Security_Manager;
   begin
      if Manager = null then
         Log.Error ("There is no security manager configured");
         raise Start_Error with "No security manager";
      end if;

      --  Start the security manager.
      AWA.Permissions.Services.Permission_Manager'Class (Manager.all).Start;

      --  Start the event service.
      App.Events.Start;

      --  Start the application.
      ASF.Applications.Main.Application (App).Start;

      --  Dump the route and filters to help in configuration issues.
      App.Dump_Routes (Util.Log.INFO_LEVEL);
   end Start;

   --  ------------------------------
   --  Close the application.
   --  ------------------------------
   overriding
   procedure Close (App : in out Application) is
   begin
      App.Events.Stop;
      ASF.Applications.Main.Application (App).Close;
   end Close;

   --  ------------------------------
   --  Register the module in the application
   --  ------------------------------
   procedure Register (App     : in Application_Access;
                       Module  : access AWA.Modules.Module'Class;
                       Name    : in String;
                       URI     : in String := "") is
   begin
      App.Register (Module.all'Unchecked_Access, Name, URI);
   end Register;

   --  ------------------------------
   --  Set a static query loader to load SQL queries.
   --  ------------------------------
   procedure Set_Query_Loader (App     : in out Application;
                               Loader  : in ADO.Queries.Static_Loader_Access) is
   begin
      App.DB_Factory.Set_Query_Loader (Loader);
   end Set_Query_Loader;

   --  ------------------------------
   --  Get the database connection for reading
   --  ------------------------------
   function Get_Session (App : Application)
                         return ADO.Sessions.Session is
   begin
      return App.DB_Factory.Get_Session;
   end Get_Session;

   --  ------------------------------
   --  Get the database connection for writing
   --  ------------------------------
   function Get_Master_Session (App : Application)
                                return ADO.Sessions.Master_Session is
   begin
      return App.DB_Factory.Get_Master_Session;
   end Get_Master_Session;

   --  ------------------------------
   --  Find the module with the given name
   --  ------------------------------
   function Find_Module (App : in Application;
                         Name : in String) return AWA.Modules.Module_Access is
   begin
      return AWA.Modules.Find_By_Name (App.Modules, Name);
   end Find_Module;

   --  ------------------------------
   --  Register the module in the application
   --  ------------------------------
   procedure Register (App     : in out Application;
                       Module  : in AWA.Modules.Module_Access;
                       Name    : in String;
                       URI     : in String := "") is
   begin
      AWA.Modules.Register (App.Modules'Unchecked_Access, App'Unchecked_Access, Module, Name, URI);
   end Register;

   --  ------------------------------
   --  Send the event in the application event queues.
   --  ------------------------------
   procedure Send_Event (App   : in Application;
                         Event : in AWA.Events.Module_Event'Class) is
   begin
      App.Events.Send (Event);
   end Send_Event;

   --  ------------------------------
   --  Execute the <tt>Process</tt> procedure with the event manager used by the application.
   --  ------------------------------
   procedure Do_Event_Manager (App : in out Application;
                               Process : access procedure
                                 (Events : in out AWA.Events.Services.Event_Manager)) is
   begin
      Process (App.Events);
   end Do_Event_Manager;

   --  ------------------------------
   --  Initialize the parser represented by <b>Parser</b> to recognize the configuration
   --  that are specific to the plugins that have been registered so far.
   --  ------------------------------
   procedure Initialize_Parser (App : in out Application'Class;
                                Parser : in out Util.Serialize.IO.Parser'Class) is
      procedure Process (Module : in out AWA.Modules.Module'Class);

      procedure Process (Module : in out AWA.Modules.Module'Class) is
      begin
         Module.Initialize_Parser (Parser);
      end Process;
   begin
      AWA.Modules.Iterate (App.Modules, Process'Access);
   end Initialize_Parser;

   --  ------------------------------
   --  Get the current application from the servlet context or service context.
   --  ------------------------------
   function Current return Application_Access is
      use type AWA.Services.Contexts.Service_Context_Access;

      Ctx : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
   begin
      if Ctx /= null then
         return Ctx.Get_Application;
      end if;

      --  If there is no service context, look in the servlet current context.
      declare
         use type ASF.Servlets.Servlet_Registry_Access;

         Ctx : constant ASF.Servlets.Servlet_Registry_Access := ASF.Server.Current;
      begin
         if Ctx = null then
            Log.Warn ("There is no service context");
            return null;
         end if;
         if not (Ctx.all in AWA.Applications.Application'Class) then
            Log.Warn ("The servlet context is not an application");
            return null;
         end if;
         return AWA.Applications.Application'Class (Ctx.all)'Access;
      end;
   end Current;

end AWA.Applications;
