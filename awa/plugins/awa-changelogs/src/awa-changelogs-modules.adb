-----------------------------------------------------------------------
--  awa-changelogs-modules -- Module changelogs
--  Copyright (C) 2014 Stephane Carrez
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
package body AWA.Changelogs.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Changelogs.Module");

   --  ------------------------------
   --  Initialize the changelogs module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Changelog_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the changelogs module");

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the changelogs module.
   --  ------------------------------
   function Get_Changelog_Module return Changelog_Module_Access is
      function Get is new AWA.Modules.Get (Changelog_Module, Changelog_Module_Access, NAME);
   begin
      return Get;
   end Get_Changelog_Module;

end AWA.Changelogs.Modules;
