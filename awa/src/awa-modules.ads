-----------------------------------------------------------------------
--  awa-modules -- Application Module
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

with Ada.Finalization;
with Util.Beans.Basic;
with Util.Beans.Objects;
with ASF.Modules;
with ASF.Events.Modules;
with ASF.Applications.Main;
with ADO.Sessions;
with AWA.Applications;
package AWA.Modules is

   --  ------------------------------
   --  Module manager
   --  ------------------------------
   --
   --  The <b>Module_Manager</b> represents the root of the logic manager
   type Module_Manager is new Ada.Finalization.Limited_Controlled
     and Util.Beans.Basic.Readonly_Bean with private;

   function Get_Value (Manager : in Module_Manager;
                       Name    : in String) return Util.Beans.Objects.Object;

   --  ------------------------------
   --  Module
   --  ------------------------------
   type Module is abstract new ASF.Modules.Module with private;
   type Module_Access is access all Module'Class;

   procedure Initialize (Manager : in out Module_Manager;
                         Module  : in AWA.Modules.Module'Class);

   --  Get the database connection for reading
   function Get_Session (Manager : Module_Manager)
                         return ADO.Sessions.Session;

   --  Get the database connection for writing
   function Get_Master_Session (Manager : Module_Manager)
     return ADO.Sessions.Master_Session;

   --  Send the event to the module.  The module identified by <b>To</b> is
   --  found and the event is posted on its event channel.
   procedure Send_Event (Manager : in Module_Manager;
                         To      : in String;
                         Content : in ASF.Events.Modules.Module_Event'Class);

   --  Get the database connection for reading
   function Get_Session (Manager : Module)
                         return ADO.Sessions.Session;

   --  Get the database connection for writing
   function Get_Master_Session (Manager : Module)
                                return ADO.Sessions.Master_Session;

   overriding
   procedure Initialize (Plugin : in out Module;
                         App    : access ASF.Applications.Main.Application'Class);

   type Pool_Module is abstract new Module with private;

   type Session_Module is abstract new Module with private;

   generic
      type Manager_Type is new Module_Manager with private;
      type Manager_Type_Access is access all Manager_Type'Class;
      Name : String;
   function Get_Manager return Manager_Type_Access;

private

   type Module_Manager is new Ada.Finalization.Limited_Controlled
     and Util.Beans.Basic.Readonly_Bean with record
      Module : Module_Access := null;
   end record;

   type Module is abstract new ASF.Modules.Module with record
      Awa_App : AWA.Applications.Application_Access := null;
      Self    : Module_Access := null;
   end record;

   type Pool_Module is new Module with record
      D : Natural;
   end record;

   type Session_Module is new Module with record
      P : Natural;
   end record;

end AWA.Modules;
