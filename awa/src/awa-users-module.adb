-----------------------------------------------------------------------
--  awa -- Ada Web Application
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

with ASF.Modules.Beans;
with ASF.Beans;

with AWA.Users.Beans;

with Util.Log.Loggers;
package body AWA.Users.Module is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Users.Module");

   package Register is new ASF.Modules.Beans (Module => User_Module,
                                              Module_Access => User_Module_Access);

   overriding
   procedure Initialize (Plugin : in out User_Module;
                         App    : access ASF.Applications.Main.Application'Class) is
   begin
      Log.Info ("Initializing the users module");

      AWA.Modules.Module (Plugin).Initialize (App);
      Plugin.Manager := Plugin.Create_User_Manager;
      Register.Register (Plugin  => Plugin,
                         Name    => "login",
                         Handler => AWA.Users.Beans.Create_Authenticate_Bean'Access,
                         Scope   => ASF.Beans.REQUEST_SCOPE);

      Register.Register (Plugin  => Plugin,
                         Name    => "register",
                         Handler => AWA.Users.Beans.Create_Authenticate_Bean'Access,
                         Scope   => ASF.Beans.REQUEST_SCOPE);

      Register.Register (Plugin  => Plugin,
                         Name    => "lostPassword",
                         Handler => AWA.Users.Beans.Create_Authenticate_Bean'Access,
                         Scope   => ASF.Beans.REQUEST_SCOPE);
   end Initialize;

   --  ------------------------------
   --  Get the user manager.
   --  ------------------------------
   function Get_User_Manager (Plugin : in User_Module) return Services.User_Manager_Access is
   begin
      return Plugin.Manager;
   end Get_User_Manager;

   --  ------------------------------
   --  Create a user manager.
   --  ------------------------------
   function Create_User_Manager (Plugin : in User_Module) return Services.User_Manager_Access is
      Result : constant Services.User_Manager_Access := new Services.User_Manager;
   begin
      Result.Initialize (Plugin);
      return Result;
   end Create_User_Manager;

   --  ------------------------------
   --  Get the user module instance associated with the current application.
   --  ------------------------------
   function Get_User_Module return User_Module_Access is
      function Get is new ASF.Modules.Get (User_Module, User_Module_Access, NAME);
   begin
      return Get;
   end Get_User_Module;

   --  ------------------------------
   --  Get the user manager instance associated with the current application.
   --  ------------------------------
   function Get_User_Manager return Services.User_Manager_Access is
      Module : constant User_Module_Access := Get_User_Module;
   begin
      if Module = null then
         Log.Error ("There is no active User_Module");
         return null;
      else
         return Module.Get_User_Manager;
      end if;
   end Get_User_Manager;

end AWA.Users.Module;
