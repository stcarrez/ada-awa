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

with Ada.IO_Exceptions;

with ASF.Beans;

with ADO.Drivers;

with EL.Contexts.Default;
with Util.Files;
with Util.Log.Loggers;

with AWA.Components.Factory;
with AWA.Applications.Factory;
with AWA.Applications.Configs;
package body AWA.Applications is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Applications.Configs");

   --  ------------------------------
   --  Initialize the application
   --  ------------------------------
   overriding
   procedure Initialize (App     : in out Application;
                         Conf    : in ASF.Applications.Config;
                         Factory : in out ASF.Applications.Main.Application_Factory'Class) is
   begin
      AWA.Applications.Factory.Set_Application (Factory, App'Unchecked_Access);
      ASF.Applications.Main.Application (App).Initialize (Conf, Factory);

      Application'Class (App).Initialize_Modules;
      App.Load_Configuration;
   end Initialize;

   --  ------------------------------
   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   --  ------------------------------
   overriding
   procedure Initialize_Servlets (App : in out Application) is
   begin
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
      ASF.Applications.Main.Application (App).Initialize_Filters;
   end Initialize_Filters;

   --  ------------------------------
   --  Initialize the ASF components provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the component factories used by the application.
   --  ------------------------------
   overriding
   procedure Initialize_Components (App : in out Application) is
   begin
      ASF.Applications.Main.Application (App).Initialize_Components;
      App.Add_Components (AWA.Components.Factory.Definition);
   end Initialize_Components;

   --  ------------------------------
   --  Initialize the application configuration properties.  Properties defined in <b>Conf</b>
   --  are expanded by using the EL expression resolver.
   --  ------------------------------
   overriding
   procedure Initialize_Config (App  : in out Application;
                                Conf : in out ASF.Applications.Config) is
   begin
      ASF.Applications.Main.Application (App).Initialize_Config (Conf);
      ADO.Drivers.Initialize (Conf);
      App.DB_Factory.Create (Conf.Get ("database"));
      AWA.Modules.Initialize (App.Modules, Conf);
   end Initialize_Config;

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
   --  Register the module in the registry.
   --  ------------------------------
   procedure Load_Configuration (App : in out Application) is
      Paths : constant String := App.Get_Config (P_Module_Dir.P);
      Base  : constant String := App.Get_Config (P_Config_File.P);
      Path  : constant String := Util.Files.Find_File_Path (Base, Paths);
      Ctx   : aliased EL.Contexts.Default.Default_Context;
   begin
      AWA.Applications.Configs.Read_Configuration (App, Path, Ctx'Unchecked_Access);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Warn ("Application configuration file '{0}' does not exist", Path);
   end Load_Configuration;

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
--        procedure Set_Beans (Factory : in out ASF.Beans.Bean_Factory);
--        procedure Set_Beans (Factory : in out ASF.Beans.Bean_Factory) is
--        begin
--           Module.Register_Factory (Factory);
--        end Set_Beans;
--
--        procedure Register_Beans is
--           new ASF.Applications.Main.Register_Beans (Set_Beans);
   begin
      --  Module.Initialize ();
      AWA.Modules.Register (App.Modules'Unchecked_Access, App'Unchecked_Access, Module, Name, URI);

--        Register_Beans (App);
--        App.View.Register_Module (Module);  SCz: 2011-08-10: must check if necessary
   end Register;

end AWA.Applications;
