-----------------------------------------------------------------------
--  awa-storages-module -- Storage management module
--  Copyright (C) 2012, 2016, 2019, 2020 Stephane Carrez
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
with Util.Log.Loggers;

with AWA.Modules.Get;
with AWA.Modules.Beans;
with AWA.Applications;
with AWA.Storages.Beans;

package body AWA.Storages.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Storages.Module");

   package Register is new AWA.Modules.Beans (Module        => Storage_Module,
                                              Module_Access => Storage_Module_Access);

   --  ------------------------------
   --  Initialize the storage module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Storage_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the storage module");

      --  Setup the resource bundles.
      App.Register ("storageMsg", "storages");

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Storages.Beans.Upload_Bean",
                         Handler => AWA.Storages.Beans.Create_Upload_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Storages.Beans.Folder_Bean",
                         Handler => AWA.Storages.Beans.Create_Folder_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Storages.Beans.Folder_List_Bean",
                         Handler => AWA.Storages.Beans.Create_Folder_List_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Storages.Beans.Storage_List_Bean",
                         Handler => AWA.Storages.Beans.Create_Storage_List_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Storages.Beans.Storage_Bean",
                         Handler => AWA.Storages.Beans.Create_Storage_Bean'Access);

      App.Add_Servlet ("storage", Plugin.Storage_Servlet'Unchecked_Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Storage_Module;
                        Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);
   begin
      --  Create the storage manager when everything is initialized.
      Plugin.Manager := Plugin.Create_Storage_Manager;
   end Configure;

   --  ------------------------------
   --  Get the storage manager.
   --  ------------------------------
   function Get_Storage_Manager (Plugin : in Storage_Module)
                                 return Services.Storage_Service_Access is
   begin
      return Plugin.Manager;
   end Get_Storage_Manager;

   --  ------------------------------
   --  Create a storage manager.  This operation can be overriden to provide another
   --  storage service implementation.
   --  ------------------------------
   function Create_Storage_Manager (Plugin : in Storage_Module)
                                    return Services.Storage_Service_Access is
      Result : constant Services.Storage_Service_Access := new Services.Storage_Service;
   begin
      Result.Initialize (Plugin);
      return Result;
   end Create_Storage_Manager;

   --  ------------------------------
   --  Get the storage module instance associated with the current application.
   --  ------------------------------
   function Get_Storage_Module return Storage_Module_Access is
      function Get is new AWA.Modules.Get (Storage_Module, Storage_Module_Access, NAME);
   begin
      return Get;
   end Get_Storage_Module;

   --  ------------------------------
   --  Get the storage manager instance associated with the current application.
   --  ------------------------------
   function Get_Storage_Manager return Services.Storage_Service_Access is
      Module : constant Storage_Module_Access := Get_Storage_Module;
   begin
      if Module = null then
         Log.Error ("There is no active Storage_Module");
         return null;
      else
         return Module.Get_Storage_Manager;
      end if;
   end Get_Storage_Manager;

end AWA.Storages.Modules;
