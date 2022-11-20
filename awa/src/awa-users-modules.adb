-----------------------------------------------------------------------
--  awa-users-model -- User management module
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2022 Stephane Carrez
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

with AWA.Modules.Beans;
with AWA.Modules.Get;

with AWA.Applications;
with AWA.Users.Beans;

with Util.Log.Loggers;
package body AWA.Users.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Users.Module");

   package Register is new AWA.Modules.Beans (Module => User_Module,
                                              Module_Access => User_Module_Access);

   --  ------------------------------
   --  Initialize the user module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out User_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the users module");

      --  Setup the resource bundles.
      App.Register ("userMsg", "users");

      --  Register the OpenID servlets.
      App.Add_Servlet (Name   => "openid-auth",
                       Server => Plugin.Auth'Unchecked_Access);
      App.Add_Servlet (Name   => "openid-verify",
                       Server => Plugin.Verify_Auth'Unchecked_Access);
      App.Add_Servlet (Name   => "verify-access-key",
                       Server => Plugin.Verify_Key'Unchecked_Access);

      --  Setup the verify access key filter.
      App.Add_Filter ("verify-access-key", Plugin.Key_Filter'Unchecked_Access);
      App.Add_Filter ("auth-filter", Plugin.Auth_Filter'Unchecked_Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Users.Beans.Authenticate_Bean",
                         Handler => AWA.Users.Beans.Create_Authenticate_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Users.Beans.Current_User_Bean",
                         Handler => AWA.Users.Beans.Create_Current_User_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out User_Module;
                        Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);
   begin
      --  Create the user manager when everything is initialized.
      Plugin.Manager := Plugin.Create_User_Manager;
   end Configure;

   --  ------------------------------
   --  Get the user manager.
   --  ------------------------------
   function Get_User_Manager (Plugin : in User_Module) return Services.User_Service_Access is
   begin
      return Plugin.Manager;
   end Get_User_Manager;

   --  ------------------------------
   --  Create a user manager.  This operation can be overridden to provide another
   --  user service implementation.
   --  ------------------------------
   function Create_User_Manager (Plugin : in User_Module) return Services.User_Service_Access is
      Result : constant Services.User_Service_Access := new Services.User_Service;
   begin
      Result.Initialize (Plugin);
      return Result;
   end Create_User_Manager;

   --  ------------------------------
   --  Get the user module instance associated with the current application.
   --  ------------------------------
   function Get_User_Module return User_Module_Access is
      function Get is new AWA.Modules.Get (User_Module, User_Module_Access, NAME);
   begin
      return Get;
   end Get_User_Module;

   --  ------------------------------
   --  Get the user manager instance associated with the current application.
   --  ------------------------------
   function Get_User_Manager return Services.User_Service_Access is
      Module : constant User_Module_Access := Get_User_Module;
   begin
      if Module = null then
         Log.Error ("There is no active User_Module");
         return null;
      else
         return Module.Get_User_Manager;
      end if;
   end Get_User_Manager;

end AWA.Users.Modules;
