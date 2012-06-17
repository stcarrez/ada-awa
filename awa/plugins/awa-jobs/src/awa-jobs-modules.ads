-----------------------------------------------------------------------
--  awa-jobs-module -- Job module
--  Copyright (C) 2012 Stephane Carrez
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

with ASF.Applications;

with AWA.Modules;
with AWA.Jobs.Services;

--  == Job Module ==
--  The <b>Jobs.Modules</b> is the entry point for the management of asynchronous jobs.
--  It maintains a list of job types that can be executed for the application and it
--  manages the job dispatchers.
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

   --  Registers the job work procedure represented by <b>Work</b> under the name <b>Name</b>.
   procedure Register (Plugin : in out Job_Module;
                       Work   : in AWA.Jobs.Services.Work_Access;
                       Name   : in String);

private

   type Job_Module is new AWA.Modules.Module with null record;

end AWA.Jobs.Modules;