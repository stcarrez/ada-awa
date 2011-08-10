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

--  with AWA.Messages;
with ASF.Applications.Main;
with ADO.Sessions.Factory;
with AWA.Modules;
package AWA.Applications is

   --  Module manager
   --
   --  The <b>Module_Manager</b> represents the root of the logic manager

   type Application is new ASF.Applications.Main.Application with private;
   type Application_Access is access all Application'Class;

   --  Initialize the application
   overriding
   procedure Initialize (App  : in out Application;
                         Conf : in ASF.Applications.Config;
                         Factory : in ASF.Applications.Main.Application_Factory'Class);

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

private

   type Application is new ASF.Applications.Main.Application with record
      DB_Factory : ADO.Sessions.Factory.Session_Factory;
      Modules    : aliased AWA.Modules.Module_Registry;
   end record;

end AWA.Applications;
