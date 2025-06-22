-----------------------------------------------------------------------
--  awa-settings --  Settings module
--  Copyright (C) 2013, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  = Settings Module =
--  The `Settings` module provides management of application and user settings.
--  A setting is identified by a unique name in the application.  It is saved in
--  the database and associated with a user.
--
--  == Getting a user setting ==
--  Getting a user setting is as simple as calling a function with the setting name
--  and the default value.  If the setting was modified by the user and saved in the
--  database, the saved value will be returned.  Otherwise, the default value is returned.
--  For example, if an application defines a `row-per-page` setting to define how
--  many rows are defined in a list, the user setting can be retrieved with:
--
--    Row_Per_Page : constant Integer := AWA.Settings.Get_User_Setting ("row-per-page", 10);
--
--  == Saving a user setting ==
--  When a user changes the setting value, we just have to save it in the database.
--  The setting value will either be updated if it exists or created.
--
--    AWA.Settings.Set_User_Setting ("row-per-page", 20);
--
--  @include awa-settings-modules.ads
--
--  == Data model ==
--  [images/awa_settings_model.png]
package AWA.Settings is

   --  Get the user setting identified by the given name.
   --  If the user does not have such setting, return the default value.
   function Get_User_Setting (Name    : in String;
                              Default : in String) return String;

   --  Get the user setting identified by the given name.
   --  If the user does not have such setting, return the default value.
   function Get_User_Setting (Name    : in String;
                              Default : in Integer) return Integer;

   --  Set the user setting identified by the given name.  If the user
   --  does not have such setting, it is created and set to the given value.
   --  Otherwise, the user setting is updated to hold the new value.
   procedure Set_User_Setting (Name   : in String;
                               Value  : in String);

   --  Set the user setting identified by the given name.  If the user
   --  does not have such setting, it is created and set to the given value.
   --  Otherwise, the user setting is updated to hold the new value.
   procedure Set_User_Setting (Name   : in String;
                               Value  : in Integer);

end AWA.Settings;
