-----------------------------------------------------------------------
--  awa-wikis-modules -- Module wikis
--  Copyright (C) 2015 Stephane Carrez
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

with AWA.Modules.Get;
with Util.Log.Loggers;
package body Awa.Wikis.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Wikis.Module");

   --  ------------------------------
   --  Initialize the wikis module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Wiki_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the wikis module");

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the wikis module.
   --  ------------------------------
   function Get_Wiki_Module return Wiki_Module_Access is
      function Get is new AWA.Modules.Get (Wiki_Module, Wiki_Module_Access, NAME);
   begin
      return Get;
   end Get_Wiki_Module;

end Awa.Wikis.Modules;
