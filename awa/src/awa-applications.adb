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

with ASF.Beans;

with AWA.Components.Factory;
with AWA.Applications.Factory;
package body AWA.Applications is

   --  ------------------------------
   --  Initialize the application
   --  ------------------------------
   overriding
   procedure Initialize (App     : in out Application;
                         Conf    : in ASF.Applications.Config;
                         Factory : in out ASF.Applications.Main.Application_Factory'Class) is
      URI     : constant String := Conf.Get ("database");
   begin
      AWA.Applications.Factory.Set_Application (Factory, App'Unchecked_Access);
      App.DB_Factory.Create (URI);
      ASF.Applications.Main.Application (App).Initialize (Conf, Factory);
      App.Add_Components (AWA.Components.Factory.Definition);
      AWA.Modules.Initialize (App.Modules, Conf);
   end Initialize;

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
      procedure Set_Beans (Factory : in out ASF.Beans.Bean_Factory);
      procedure Set_Beans (Factory : in out ASF.Beans.Bean_Factory) is
      begin
         Module.Register_Factory (Factory);
      end Set_Beans;

      procedure Register_Beans is
         new ASF.Applications.Main.Register_Beans (Set_Beans);
   begin
      Module.Initialize (App'Unchecked_Access);
      AWA.Modules.Register (App.Modules'Unchecked_Access, Module, Name, URI);

      Register_Beans (App);
--        App.View.Register_Module (Module);  SCz: 2011-08-10: must check if necessary
   end Register;

end AWA.Applications;
