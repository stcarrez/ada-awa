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
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;

with Util.Log.Loggers;
with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Events.Channels;

with ASF.Beans;
with ASF.Events.Modules;
with ASF.Applications;
with ADO.Sessions;
limited with AWA.Applications;

--  The <b>AWA.Modules</b> package defines simple pluggable modules in
--  the web application.  A module is a software component that can be
--  integrated in the application during the startup or initialization
--  phase.
--
--  A module can be attached to a given URI under the application's URI.
--  The module will handle all requests below that URI.
--
--  Each module is associated with an event channel that allows other
--  modules to post easily events.
package AWA.Modules is

   type Application_Access is access all AWA.Applications.Application'Class;

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
   type Module is abstract new Ada.Finalization.Limited_Controlled with private;
   type Module_Access is access all Module'Class;

   --  Get the module name
   function Get_Name (Plugin : Module) return String;

   --  Get the base URI for this module
   function Get_URI (Plugin : Module) return String;

   --  Get the application in which this module is registered.
   function Get_Application (Plugin : in Module) return Application_Access;

   --  Find the module with the given name
   function Find_Module (Plugin : Module;
                         Name   : String) return Module_Access;

   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   function Get_Config (Plugin  : Module;
                        Name    : String;
                        Default : String := "") return String;

   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   function Get_Config (Plugin  : Module;
                        Name    : String;
                        Default : Integer := -1) return Integer;

   procedure Initialize (Manager : in out Module_Manager;
                         Module  : in AWA.Modules.Module'Class);

   procedure Initialize (Plugin : in out Module;
                         App    : in Application_Access);

   --  Get the event subscribers for a given event name.
   function Get_Subscribers (Plugin : in Module;
                             Event  : in String) return String;

   --  Send the event to the module.  The module identified by <b>To</b> is
   --  found and the event is posted on its event channel.
   procedure Send_Event (Plugin  : in Module;
                         To      : in String;
                         Content : in ASF.Events.Modules.Module_Event'Class);

   --  Receive an event sent by another module with <b>Send_Event</b> method.
   procedure Receive_Event (Plugin  : in out Module;
                            Content : in ASF.Events.Modules.Module_Event'Class);

   --  Get the database connection for reading
   function Get_Session (Manager : Module)
                         return ADO.Sessions.Session;

   --  Get the database connection for writing
   function Get_Master_Session (Manager : Module)
                                return ADO.Sessions.Master_Session;

   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   procedure Register (Plugin : in out Module;
                       Name    : in String;
                       Bind    : in ASF.Beans.Class_Binding_Access);

   --  Finalize the module.
   overriding
   procedure Finalize (Plugin : in out Module);

   type Pool_Module is abstract new Module with private;

   type Session_Module is abstract new Module with private;

   generic
      type Manager_Type is new Module_Manager with private;
      type Manager_Type_Access is access all Manager_Type'Class;
      Name : String;
   function Get_Manager return Manager_Type_Access;

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

   --  ------------------------------
   --  Module Registry
   --  ------------------------------
   --  The module registry maintains the list of available modules with
   --  operations to retrieve them either from a name or from the base URI.
   type Module_Registry is limited private;
   type Module_Registry_Access is access all Module_Registry;

   --  Initialize the registry
   procedure Initialize (Registry : in out Module_Registry;
                         Config   : in ASF.Applications.Config);

   --  Register the module in the registry.
   procedure Register (Registry : in Module_Registry_Access;
                       App      : in Application_Access;
                       Plugin   : in Module_Access;
                       Name     : in String;
                       URI      : in String);

   --  Find the module with the given name
   function Find_By_Name (Registry : Module_Registry;
                          Name     : String) return Module_Access;

   --  Find the module mapped to a given URI
   function Find_By_URI (Registry : Module_Registry;
                         URI      : String) return Module_Access;

private

   use Ada.Strings.Unbounded;

   --  Event channel subscriber
   type Module_Subscriber is new Util.Events.Channels.Subscriber with record
      Module : Module_Access;
   end record;

   --  Receive an event from the event channel
   procedure Receive_Event (Sub  : in out Module_Subscriber;
                            Item : in Util.Events.Event'Class);

   type Module is abstract new Ada.Finalization.Limited_Controlled with record
      Registry   : Module_Registry_Access;
      Subscriber : aliased Module_Subscriber;
      App        : Application_Access := null;
      Channel    : Util.Events.Channels.Channel_Access;
      Name       : Unbounded_String;
      URI        : Unbounded_String;
      Config     : ASF.Applications.Config;
      Self       : Module_Access := null;
   end record;

   --  Map to find a module from its name or its URI
   package Module_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                            Element_Type    => Module_Access,
                                            Hash            => Ada.Strings.Hash,
                                            Equivalent_Keys => "=");

   type Module_Registry is limited record
      Config   : ASF.Applications.Config;
      Name_Map : Module_Maps.Map;
      URI_Map  : Module_Maps.Map;
   end record;

   type Module_Manager is new Ada.Finalization.Limited_Controlled
     and Util.Beans.Basic.Readonly_Bean with record
      Module : Module_Access := null;
   end record;

   type Pool_Module is new Module with record
      D : Natural;
   end record;

   type Session_Module is new Module with record
      P : Natural;
   end record;

   use Util.Log;

   --  The logger (used by the generic Get function).
   Log : constant Loggers.Logger := Loggers.Create ("AWA.Modules");

end AWA.Modules;
