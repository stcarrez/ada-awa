-----------------------------------------------------------------------
--  awa-settings --  Settings module
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
