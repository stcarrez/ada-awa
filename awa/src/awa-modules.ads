-----------------------------------------------------------------------
--  awa-modules -- Application Module
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

with Ada.Finalization;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;

with Util.Log.Loggers;
with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Serialize.IO;
with Util.Listeners;

with EL.Expressions;

with ASF.Beans;
with ASF.Applications;
with ADO.Sessions;

with AWA.Events;
limited with AWA.Applications;

--  == AWA Modules ==
--  A module is a software component that can be integrated in the
--  web application.  The module can bring a set of service APIs,
--  some Ada beans and some presentation files.  The AWA framework
--  allows to configure various parts of a module when it is integrated
--  in an application.  Some modules are designed to be re-used by
--  several applications (for example a _mail_ module, a _users_
--  module, ...).  Other modules could be specific to an application.
--  An application will be made of several modules, some will be
--  generic some others specific to the application.
--
--  === Registration ===
--  The module should have only one instance per application and it must
--  be registered when the application is initialized.  The module
--  instance should be added to the application record as follows:
--
--    type Application is new AWA.Applications.Application with record
--       Xxx       : aliased Xxx_Module;
--    end record;
--
--  The application record must override the `Initialize_Module` procedure
--  and it must register the module instance.  This is done as follows:
--
--    overriding
--    procedure Initialize_Modules (App : in out Application) is
--    begin
--       Register (App    => App.Self.all'Access,
--                 Name   => Xxx.Module.NAME,
--                 URI    => "xxx",
--                 Module => App.User_Module'Access);
--    end Initialize_Modules;
--
--  The module is registered under a unique name.  That name is used
--  to load the module configuration.
--
--  === Configuration ===
--  The module is configured by using an XML or a properties file.
--  The configuration file is used to define:
--
--    * the Ada beans that the module defines and uses,
--    * the events that the module wants to receive and the action
--      that must be performed when the event is posted,
--    * the permissions that the module needs and how to check them,
--    * the navigation rules which are used for the module web interface,
--    * the servlet and filter mappings used by the module
--
--  The module configuration is located in the *config* directory
--  and must be the name of the module followed by the file extension
--  (example: `module-name`.xml or `module-name`.properties).
--
--
package AWA.Modules is

   type Application_Access is access all AWA.Applications.Application'Class;

   --  ------------------------------
   --  Module manager
   --  ------------------------------
   --
   --  The `Module_Manager` represents the root of the logic manager
   type Module_Manager is new Ada.Finalization.Limited_Controlled
     and Util.Beans.Basic.Readonly_Bean with private;

   function Get_Value (Manager : in Module_Manager;
                       Name    : in String) return Util.Beans.Objects.Object;

   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   function Get_Config (Plugin  : Module_Manager;
                        Name    : String;
                        Default : String := "") return String;

   --  Get the module configuration property identified by the `Config` parameter.
   --  If the property does not exist, the default configuration value is returned.
   function Get_Config (Plugin : in Module_Manager;
                        Config : in ASF.Applications.Config_Param) return String;
   function Get_Config (Plugin : in Module_Manager;
                        Config : in ASF.Applications.Config_Param) return Integer;
   function Get_Config (Plugin : in Module_Manager;
                        Config : in ASF.Applications.Config_Param) return Boolean;

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

   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   function Get_Config (Plugin  : Module;
                        Name    : String;
                        Default : Boolean := False) return Boolean;

   --  Get the module configuration property identified by the `Config` parameter.
   --  If the property does not exist, the default configuration value is returned.
   function Get_Config (Plugin : in Module;
                        Config : in ASF.Applications.Config_Param) return String;
   function Get_Config (Plugin : in Module;
                        Config : in ASF.Applications.Config_Param) return Integer;
   function Get_Config (Plugin : in Module;
                        Config : in ASF.Applications.Config_Param) return Boolean;

   --  Get the module configuration property identified by the `Config` parameter.
   --  If the configuration property does not exist, returns the default value.
   function Get_Config (Plugin  : in Module;
                        Name    : in String;
                        Default : in String := "")
                        return EL.Expressions.Expression;

   procedure Initialize (Manager : in out Module_Manager;
                         Module  : in AWA.Modules.Module'Class);

   procedure Initialize (Plugin : in out Module;
                         App    : in Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Initialize the configuration file parser represented by `Parser` to recognize
   --  the specific configuration recognized by the module.
   procedure Initialize_Parser (Plugin : in out Module;
                                Parser : in out Util.Serialize.IO.Parser'Class) is null;

   --  Configures the module after its initialization and after having read its XML configuration.
   procedure Configure (Plugin : in out Module;
                        Props  : in ASF.Applications.Config) is null;

   --  Send the event to the module.  The module identified by `To` is
   --  found and the event is posted on its event channel.
   procedure Send_Event (Plugin  : in Module;
                         Content : in AWA.Events.Module_Event'Class);

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

   --  Add a listener to the module listner list.  The module will invoke the listner
   --  depending on events or actions that occur in the module.
   procedure Add_Listener (Into : in out Module;
                           Item : in Util.Listeners.Listener_Access);

   --  Find the module with the given name in the application and add the listener to the
   --  module listener list.
   procedure Add_Listener (Plugin : in Module;
                           Name   : in String;
                           Item   : in Util.Listeners.Listener_Access);

   --  Remove a listener from the module listener list.
   procedure Remove_Listener (Into : in out Module;
                              Item : in Util.Listeners.Listener_Access);

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

   --  Send the event to the module.  The module identified by `To` is
   --  found and the event is posted on its event channel.
   procedure Send_Event (Manager : in Module_Manager;
                         Content : in AWA.Events.Module_Event'Class);

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

   --  Iterate over the modules that have been registered and execute the `Process`
   --  procedure on each of the module instance.
   procedure Iterate (Registry : in Module_Registry;
                      Process  : access procedure (Plugin : in out Module'Class));

private

   use Ada.Strings.Unbounded;

   type Module is abstract new Ada.Finalization.Limited_Controlled with record
      Registry   : Module_Registry_Access;
      App        : Application_Access := null;
      Name       : Unbounded_String;
      URI        : Unbounded_String;
      Config     : ASF.Applications.Config;
      Self       : Module_Access := null;
      Listeners  : Util.Listeners.List;
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
