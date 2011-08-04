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

with AWA.Modules;
with AWA.Components.Factory;
package body AWA.Applications is

   --  ------------------------------
   --  Initialize the application
   --  ------------------------------
   overriding
   procedure Initialize (App  : in out Application;
                         Conf : in ASF.Applications.Config;
                         Factory : in ASF.Applications.Main.Application_Factory'Class) is
      URI : constant String := Conf.Get ("database");
   begin
      ASF.Applications.Main.Application (App).Initialize (Conf, Factory);
      App.Add_Components (AWA.Components.Factory.Definition);
      App.Factory.Create (URI);
   end Initialize;

   --  ------------------------------
   --  Register the module in the application
   --  ------------------------------
   procedure Register (App     : in Application_Access;
                       Module  : access AWA.Modules.Module'Class;
                       Name    : in String;
                       URI     : in String := "") is
   begin
--        Module.Initialize (App);
      App.Register (Module.all'Unchecked_Access, Name, URI);
   end Register;

   --  ------------------------------
   --  Get the database connection for reading
   --  ------------------------------
   function Get_Session (App : Application)
                         return ADO.Sessions.Session is
   begin
      return App.Factory.Get_Session;
   end Get_Session;

   --  ------------------------------
   --  Get the database connection for writing
   --  ------------------------------
   function Get_Master_Session (App : Application)
                                return ADO.Sessions.Master_Session is
   begin
      return App.Factory.Get_Master_Session;
   end Get_Master_Session;

end AWA.Applications;
