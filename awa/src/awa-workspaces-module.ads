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

with AWA.Modules;
package Awa.Workspaces.Module is

   --  The name under which the module is registered.
   NAME : constant String := "workspaces";

   --  ------------------------------
   --  Module workspaces
   --  ------------------------------
   type Workspaces_Module is new AWA.Modules.Module with private;
   type Workspaces_Module_Access is access all Workspaces_Module'Class;

   --  Initialize the workspaces module.
   overriding
   procedure Initialize (Plugin : in out Workspaces_Module;
                         App    : in AWA.Modules.Application_Access);

private

   type Workspaces_Module is new AWA.Modules.Module with null record;

end Awa.Workspaces.Module;
