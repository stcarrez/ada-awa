-----------------------------------------------------------------------
--  awa-jobs-module -- Job module
--  Copyright (C) 2012, 2013 Stephane Carrez
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
with Ada.Tags;
with Ada.Calendar;

with Util.Log.Loggers;

with ADO.Sessions;
with ADO.SQL;

with AWA.Applications;
with AWA.Events.Services;
with AWA.Modules.Get;
with AWA.Modules.Beans;
with AWA.Jobs.Beans;

package body AWA.Jobs.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Jobs.Modules");

   package Register_Beans is new AWA.Modules.Beans (Module        => Job_Module,
                                                    Module_Access => Job_Module_Access);

   --  ------------------------------
   --  Initialize the job module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Job_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
      procedure Process (Events : in out AWA.Events.Services.Event_Manager);

      Name : constant String := Props.Get ("jobs_queue", "default");

      procedure Process (Events : in out AWA.Events.Services.Event_Manager) is
      begin
         null;
      end Process;

   begin
      Log.Info ("Initializing the jobs module, queue {0}", Name);

      App.Do_Event_Manager (Process'Access);

      declare
         DB    : ADO.Sessions.Session := App.Get_Session;
         Query : ADO.SQL.Query;
         Found : Boolean;
      begin
         Query.Set_Filter ("o.name = ?");
         Query.Add_Param ("job-create");
         Plugin.Message_Type.Find (Session => DB,
                                   Query   => Query,
                                   Found   => Found);
         if not Found then
            Log.Error ("Event {0} not found in database", "job-create");
         end if;
      end;
      Register_Beans.Register (Plugin  => Plugin,
                               Name    => "AWA.Jobs.Beans.Process_Bean",
                               Handler => AWA.Jobs.Beans.Create_Process_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Get the job module instance associated with the current application.
   --  ------------------------------
   function Get_Job_Module return Job_Module_Access is
      function Get is new AWA.Modules.Get (Job_Module, Job_Module_Access, NAME);
   begin
      return Get;
   end Get_Job_Module;

   --  ------------------------------
   --  Registers the job work procedure represented by <b>Work</b> under the name <b>Name</b>.
   --  ------------------------------
   procedure Register (Plugin     : in out Job_Module;
                       Definition : in AWA.Jobs.Services.Job_Factory_Access) is
      Name  : constant String := Ada.Tags.Expanded_Name (Definition.all'Tag);
      Ename : constant String := Ada.Tags.External_Tag (Definition.all'Tag);
   begin
      Log.Info ("Register job {0} - {1}", Name, Ename);

      Plugin.Factory.Include (Name, Definition);
   end Register;

   --  ------------------------------
   --  Find the job work factory registered under the name <b>Name</b>.
   --  Returns null if there is no such factory.
   --  ------------------------------
   function Find_Factory (Plugin : in Job_Module;
                          Name   : in String) return AWA.Jobs.Services.Job_Factory_Access is
      Pos : constant Job_Factory_Map.Cursor := Plugin.Factory.Find (Name);
   begin
      if Job_Factory_Map.Has_Element (Pos) then
         return Job_Factory_Map.Element (Pos);
      else
         return null;
      end if;
   end Find_Factory;

   --  ------------------------------
   --  Create an event to schedule the job execution.
   --  ------------------------------
   procedure Create_Event (Event : in out AWA.Events.Models.Message_Ref) is
      M : constant Job_Module_Access := Get_Job_Module;
   begin
      Event.Set_Status (AWA.Events.Models.QUEUED);
      Event.Set_Message_Type (M.Message_Type);
      Event.Set_Queue (M.Queue);
      Event.Set_Create_Date (Ada.Calendar.Clock);
   end Create_Event;

end AWA.Jobs.Modules;