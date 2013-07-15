-----------------------------------------------------------------------
--  awa-awa-settings-modules -- Module awa-settings
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
with Ada.Unchecked_Deallocation;

with AWA.Services.Contexts;
with AWA.Modules.Get;
with Util.Log.Loggers;
with Util.Beans.Objects;
with Util.Beans.Basic;
package body AWA.Settings.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Awa-settings.Module");

   package ASC renames AWA.Services.Contexts;

   --  Set the user setting with the given name in the setting manager cache
   --  and in the database.
   procedure Set (Manager : in out Setting_Manager;
                  Name    : in String;
                  Value   : in String) is
   begin
      Manager.Data.Set (Name, Value);
   end Set;

   --  Get the user setting with the given name from the setting manager cache.
   --  Load the user setting from the database if necessary.
   procedure Get (Manager : in out Setting_Manager;
                  Name    : in String;
                  Default : in String;
                  Value   : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Manager.Data.Get (Name, Default, Value);
   end Get;

   function Current return Setting_Manager_Access is
      Ctx : ASC.Service_Context_Access := ASC.Current;
      Obj : Util.Beans.Objects.Object := Ctx.Get_Session_Attribute ("AWA.Settings");
      Bean : access Util.Beans.Basic.Readonly_Bean'Class := Util.Beans.Objects.To_Bean (Obj);
   begin
      if Bean = null or else not (Bean.all in Setting_Manager'Class) then
         declare
            Mgr : Setting_Manager_Access := new Setting_Manager;
         begin
            Obj := Util.Beans.Objects.To_Object (Mgr.all'Access);
            Ctx.Set_Session_Attribute ("AWA.Settings", Obj);
            return Mgr;
         end;
      else
         return Setting_Manager'Class (Bean.all)'Access;
      end if;
   end Current;

   --  ------------------------------
   --  Initialize the awa-settings module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Setting_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the awa-settings module");

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the awa-settings module.
   --  ------------------------------
   function Get_Setting_Module return Setting_Module_Access is
      function Get is new AWA.Modules.Get (Setting_Module, Setting_Module_Access, NAME);
   begin
      return Get;
   end Get_Setting_Module;

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
                  First := Item;
               end if;
               return;
            end if;
            Previous := Item;
            Item := Item.Next_Setting;
         end loop;
         Value := Ada.Strings.Unbounded.To_Unbounded_String (Default);
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
                  First := Item;
               end if;
               if Item.Value = Value then
                  return;
               end if;
               Item.Value := Ada.Strings.Unbounded.To_Unbounded_String (Value);
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
