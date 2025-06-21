-----------------------------------------------------------------------
--  awa-settings-modules -- Module awa-settings
--  Copyright (C) 2013, 2015, 2016, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Applications;
with Ada.Strings.Unbounded;
with AWA.Modules;

--  == Integration ==
--  The `Setting_Module` manages the application and user settings.  An instance of the
--  the `Setting_Module` must be declared and registered in the AWA application.
--  The module instance can be defined as follows:
--
--    type Application is new AWA.Applications.Application with record
--       Setting_Module : aliased AWA.Settings.Modules.Setting_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Settings.Modules.NAME,
--              URI    => "settings",
--              Module => App.Setting_Module'Access);
package AWA.Settings.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "settings";

   --  The name of the HTTP session attribute that contains user settings.
   SESSION_ATTR_NAME : constant String := "AWA.Settings";

   --  The settings manager controls the creation and update of user settings.
   --  It maintains a small LRU cache of user settings that have been loaded and
   --  used by the application.  The settings manager is intended to be stored as
   --  an HTTP session attribute.
   type Setting_Manager is new AWA.Modules.Module_Manager with private;
   type Setting_Manager_Access is access all Setting_Manager'Class;

   --  Set the user setting with the given name in the setting manager cache
   --  and in the database.
   procedure Set (Manager : in out Setting_Manager;
                  Name    : in String;
                  Value   : in String);

   --  Get the user setting with the given name from the setting manager cache.
   --  Load the user setting from the database if necessary.
   procedure Get (Manager : in out Setting_Manager;
                  Name    : in String;
                  Default : in String;
                  Value   : out Ada.Strings.Unbounded.Unbounded_String);

   --  Release the memory allocated for the settings.
   overriding
   procedure Finalize (Manager : in out Setting_Manager);

   --  Get the current setting manager for the current user.
   function Current return Setting_Manager_Access;

   --  ------------------------------
   --  Module awa-settings
   --  ------------------------------
   type Setting_Module is new AWA.Modules.Module with private;
   type Setting_Module_Access is access all Setting_Module'Class;

   --  Initialize the settings module.
   overriding
   procedure Initialize (Plugin : in out Setting_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the settings module.
   function Get_Setting_Module return Setting_Module_Access;

private

   type Setting_Module is new AWA.Modules.Module with null record;

   type Setting_Data;
   type Setting_Data_Access is access all Setting_Data;

   type Setting_Data is limited record
      Next_Setting : Setting_Data_Access;
      Name         : Ada.Strings.Unbounded.Unbounded_String;
      Value        : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   protected type Settings is

      procedure Get (Name    : in String;
                     Default : in String;
                     Value   : out Ada.Strings.Unbounded.Unbounded_String);

      procedure Set (Name  : in String;
                     Value : in String);

      procedure Clear;

   private
      First   : Setting_Data_Access := null;
   end Settings;

   type Setting_Manager is new AWA.Modules.Module_Manager with record
      Data : Settings;
   end record;

end AWA.Settings.Modules;
