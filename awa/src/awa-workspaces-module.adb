-----------------------------------------------------------------------
--  awa-workspaces-module -- Module workspaces
--  Copyright (C) 2011 Stephane Carrez
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
with Util.Log.loggers;
with Awa.Workspaces.Beans;
package body Awa.Workspaces.Module is

   Log : constant Util.log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Workspaces.Module");

   package Register is new AWA.Modules.Beans (Module => Workspaces_Module,
                                              Module_Access => Workspaces_Module_Access);

   --  ------------------------------
   --  Initialize the workspaces module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Workspaces_Module;
                         App    : in AWA.Modules.Application_Access) is
   begin
      Log.Info ("Initializing the workspaces module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "Awa.Workspaces.Beans.Workspaces_Bean",
                         Handler => Awa.Workspaces.Beans.Create_Workspaces_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App);

      --  Add here the creation of manager instances.
   end Initialize;

end Awa.Workspaces.Module;
