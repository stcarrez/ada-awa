-----------------------------------------------------------------------
--  awa-settings --  Settings module
--  Copyright (C) 2013 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Util.Strings;
with AWA.Settings.Modules;
package body AWA.Settings is

   --  ------------------------------
   --  Get the user setting identified by the given name.
   --  If the user does not have such setting, return the default value.
   --  ------------------------------
   function Get_User_Setting (Name    : in String;
                              Default : in String) return String is
      Mgr   : constant AWA.Settings.Modules.Setting_Manager_Access := AWA.Settings.Modules.Current;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Mgr.Get (Name, Default, Value);
      return Ada.Strings.Unbounded.To_String (Value);
   end Get_User_Setting;

   --  ------------------------------
   --  Get the user setting identified by the given name.
   --  If the user does not have such setting, return the default value.
   --  ------------------------------
   function Get_User_Setting (Name    : in String;
                              Default : in Integer) return Integer is
      Mgr   : constant AWA.Settings.Modules.Setting_Manager_Access := AWA.Settings.Modules.Current;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Mgr.Get (Name, Integer'Image (Default), Value);
      return Integer'Value (Ada.Strings.Unbounded.To_String (Value));
   end Get_User_Setting;

   --  ------------------------------
   --  Set the user setting identified by the given name.  If the user
   --  does not have such setting, it is created and set to the given value.
   --  Otherwise, the user setting is updated to hold the new value.
   --  ------------------------------
   procedure Set_User_Setting (Name   : in String;
                               Value  : in String) is
      Mgr : constant AWA.Settings.Modules.Setting_Manager_Access := AWA.Settings.Modules.Current;
   begin
      Mgr.Set (Name, Value);
   end Set_User_Setting;

   --  ------------------------------
   --  Set the user setting identified by the given name.  If the user
   --  does not have such setting, it is created and set to the given value.
   --  Otherwise, the user setting is updated to hold the new value.
   --  ------------------------------
   procedure Set_User_Setting (Name   : in String;
                               Value  : in Integer) is
      Mgr : constant AWA.Settings.Modules.Setting_Manager_Access := AWA.Settings.Modules.Current;
   begin
      Mgr.Set (Name, Util.Strings.Image (Value));
   end Set_User_Setting;

end AWA.Settings;
