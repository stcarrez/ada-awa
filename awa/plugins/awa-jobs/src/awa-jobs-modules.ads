-----------------------------------------------------------------------
--  awa-jobs-modules -- Job module
--  Copyright (C) 2012, 2020 Stephane Carrez
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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with ASF.Applications;

with AWA.Modules;
with AWA.Jobs.Services;

--  == Integration ==
--  To be able to use the `jobs` module, you will need to add the
--  following line in your GNAT project file:
--
--    with "awa_jobs";
--
--  An instance of the `Job_Module` must be declared and registered in the
--  AWA application.  The module instance can be defined as follows:
--
--    with AWA.Jobs.Modules;
--    ...
--    type Application is new AWA.Applications.Application with record
--       Job_Module : aliased AWA.Jobs.Modules.Job_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Jobs.Modules.NAME,
--              Module => App.Job_Module'Access);
--
package AWA.Jobs.Modules is

   NAME : constant String := "jobs";

   type Job_Module is new AWA.Modules.Module with private;
   type Job_Module_Access is access all Job_Module'Class;

   --  Initialize the job module.
   overriding
   procedure Initialize (Plugin : in out Job_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the job module instance associated with the current application.
   function Get_Job_Module return Job_Module_Access;

   --  Registers the job work procedure represented by `Work` under the name `Name`.
   procedure Register (Plugin     : in out Job_Module;
                       Definition : in AWA.Jobs.Services.Job_Factory_Access);

   --  Find the job work factory registered under the name `Name`.
   --  Returns null if there is no such factory.
   function Find_Factory (Plugin : in Job_Module;
                          Name   : in String) return AWA.Jobs.Services.Job_Factory_Access;

private

   use AWA.Jobs.Services;

   --  A factory map to create job instances.
   package Job_Factory_Map is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Job_Factory_Access,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   type Job_Module is new AWA.Modules.Module with record
      Factory      : Job_Factory_Map.Map;
   end record;

end AWA.Jobs.Modules;
