-----------------------------------------------------------------------
--  awa-settings-modules -- Module awa-settings
--  Copyright (C) 2013, 2016, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

with ADO.Sessions;
with ADO.Queries;

with AWA.Services.Contexts;
with AWA.Settings.Models;
with AWA.Modules.Get;
with AWA.Users.Models;

with Util.Log.Loggers;
with Util.Beans.Objects;
with Util.Beans.Basic;
package body AWA.Settings.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Settings.Module");

   package ASC renames AWA.Services.Contexts;

   --  Load the setting value for the current user.
   --  Return the default value if there is no such setting.
   procedure Load (Name    : in String;
                   Default : in String;
                   Value   : out Ada.Strings.Unbounded.Unbounded_String);

   --  Save the setting value for the current user.
   procedure Save (Name  : in String;
                   Value : in String);

   --  ------------------------------
   --  Set the user setting with the given name in the setting manager cache
   --  and in the database.
   --  ------------------------------
   procedure Set (Manager : in out Setting_Manager;
                  Name    : in String;
                  Value   : in String) is
   begin
      Manager.Data.Set (Name, Value);
   end Set;

   --  ------------------------------
   --  Get the user setting with the given name from the setting manager cache.
   --  Load the user setting from the database if necessary.
   --  ------------------------------
   procedure Get (Manager : in out Setting_Manager;
                  Name    : in String;
                  Default : in String;
                  Value   : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Manager.Data.Get (Name, Default, Value);
   end Get;

   --  ------------------------------
   --  Release the memory allocated for the settings.
   --  ------------------------------
   overriding
   procedure Finalize (Manager : in out Setting_Manager) is
   begin
      Manager.Data.Clear;
   end Finalize;

   --  ------------------------------
   --  Get the current setting manager for the current user.
   --  ------------------------------
   function Current return Setting_Manager_Access is
      Ctx  : constant ASC.Service_Context_Access := ASC.Current;
      Obj  : Util.Beans.Objects.Object := Ctx.Get_Session_Attribute (SESSION_ATTR_NAME);
      Bean : constant access Util.Beans.Basic.Readonly_Bean'Class
        := Util.Beans.Objects.To_Bean (Obj);
   begin
      if Bean = null or else not (Bean.all in Setting_Manager'Class) then
         declare
            Mgr : constant Setting_Manager_Access := new Setting_Manager;
         begin
            Obj := Util.Beans.Objects.To_Object (Mgr.all'Access);
            Ctx.Set_Session_Attribute (SESSION_ATTR_NAME, Obj);
            return Mgr;
         end;
      else
         return Setting_Manager'Class (Bean.all)'Unchecked_Access;
      end if;
   end Current;

   --  ------------------------------
   --  Initialize the settings module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Setting_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the settings module");

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Get the settings module.
   --  ------------------------------
   function Get_Setting_Module return Setting_Module_Access is
      function Get is new AWA.Modules.Get (Setting_Module, Setting_Module_Access, NAME);
   begin
      return Get;
   end Get_Setting_Module;

   --  ------------------------------
   --  Load the setting value for the current user.
   --  Return the default value if there is no such setting.
   --  ------------------------------
   procedure Load (Name    : in String;
                   Default : in String;
                   Value   : out Ada.Strings.Unbounded.Unbounded_String) is
      Ctx     : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      DB      : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query   : ADO.Queries.Context;
      Setting : AWA.Settings.Models.User_Setting_Ref;
      Found   : Boolean;
   begin
      Query.Set_Join ("INNER JOIN awa_setting AS a ON o.setting_id = a.id ");
      Query.Set_Filter ("a.name = :name AND o.user_id = :user");
      Query.Bind_Param ("name", Name);
      Query.Bind_Param ("user", User.Get_Id);
      Setting.Find (DB, Query, Found);
      if not Found then
         Value := Ada.Strings.Unbounded.To_Unbounded_String (Default);
      else
         Value := Setting.Get_Value;
      end if;
   end Load;

   --  Save the setting value for the current user.
   procedure Save (Name  : in String;
                   Value : in String) is
      Ctx     : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      DB      : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Setting : AWA.Settings.Models.User_Setting_Ref;
      Query   : ADO.Queries.Context;
      Found   : Boolean;
   begin
      Ctx.Start;
      Query.Set_Join ("INNER JOIN awa_setting AS a ON o.setting_id = a.id ");
      Query.Set_Filter ("a.name = :name AND o.user_id = :user");
      Query.Bind_Param ("name", Name);
      Query.Bind_Param ("user", User.Get_Id);
      Setting.Find (DB, Query, Found);
      if not Found then
         declare
            Setting_Def : AWA.Settings.Models.Setting_Ref;
         begin
            Query.Clear;
            Query.Set_Filter ("o.name = :name");
            Query.Bind_Param ("name", Name);
            Setting_Def.Find (DB, Query, Found);
            if not Found then
               Setting_Def.Set_Name (Name);
               Setting_Def.Save (DB);
            end if;
            Setting.Set_Setting (Setting_Def);
         end;
         Setting.Set_User (User);
      end if;
      Setting.Set_Value (Value);
      Setting.Save (DB);
      Ctx.Commit;
   end Save;

   procedure Free is new Ada.Unchecked_Deallocation (Object => Setting_Data,
                                                     Name   => Setting_Data_Access);

   use Ada.Strings.Unbounded;

   protected body Settings is

      procedure Get (Name    : in String;
                     Default : in String;
                     Value   : out Ada.Strings.Unbounded.Unbounded_String) is
         Item     : Setting_Data_Access := First;
         Previous : Setting_Data_Access := null;
      begin
         while Item /= null loop
            if Item.Name = Name then
               Value := Item.Value;
               if Previous /= null then
                  Previous.Next_Setting := Item.Next_Setting;
                  Item.Next_Setting := First;
                  First := Item;
               end if;
               return;
            end if;
            Previous := Item;
            Item := Item.Next_Setting;
         end loop;
         Load (Name, Default, Value);
         Item := new Setting_Data;
         Item.Name  := Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Item.Value := Value;
         Item.Next_Setting := First;
         First := Item;
      end Get;

      procedure Set (Name  : in String;
                     Value : in String) is
         Item     : Setting_Data_Access := First;
         Previous : Setting_Data_Access := null;
      begin
         while Item /= null loop
            if Item.Name = Name then
               if Previous /= null then
                  Previous.Next_Setting := Item.Next_Setting;
                  Item.Next_Setting := First;
                  First := Item;
               end if;
               if Item.Value = Value then
                  return;
               end if;
               Item.Value := Ada.Strings.Unbounded.To_Unbounded_String (Value);
               Save (Name, Value);
               return;
            end if;
            Previous := Item;
            Item := Item.Next_Setting;
         end loop;
         Item := new Setting_Data;
         Item.Name  := Ada.Strings.Unbounded.To_Unbounded_String (Name);
         Item.Value := Ada.Strings.Unbounded.To_Unbounded_String (Value);
         Item.Next_Setting := First;
         First := Item;
         Save (Name, Value);
      end Set;

      procedure Clear is
      begin
         while First /= null loop
            declare
               Item : Setting_Data_Access := First;
            begin
               First := Item.Next_Setting;
               Free (Item);
            end;
         end loop;
      end Clear;

   end Settings;

end AWA.Settings.Modules;
