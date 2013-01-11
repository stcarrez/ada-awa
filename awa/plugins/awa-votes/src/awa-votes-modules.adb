-----------------------------------------------------------------------
--  awa-votes-modules -- Module votes
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

with AWA.Modules.Beans;
with AWA.Modules.Get;
with Util.Log.Loggers;
with AWA.Votes.Beans;
package body AWA.Votes.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Votes.Module");

   package Register is new AWA.Modules.Beans (Module => Vote_Module,
                                              Module_Access => Vote_Module_Access);

   --  ------------------------------
   --  Initialize the votes module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Vote_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the votes module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Votes.Beans.Votes_Bean",
                         Handler => AWA.Votes.Beans.Create_Vote_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the votes module.
   --  ------------------------------
   function Get_Vote_Module return Vote_Module_Access is
      function Get is new AWA.Modules.Get (Vote_Module, Vote_Module_Access, NAME);
   begin
      return Get;
   end Get_Vote_Module;

end AWA.Votes.Modules;
