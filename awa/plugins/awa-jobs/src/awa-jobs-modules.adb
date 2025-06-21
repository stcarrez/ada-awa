-----------------------------------------------------------------------
--  awa-jobs-modules -- Job module
--  Copyright (C) 2012, 2013, 2020, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Tags;

with Util.Log.Loggers;

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
   begin
      Log.Info ("Initializing the jobs module");

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
   --  Registers the job work procedure represented by `Work` under the name `Name`.
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
   --  Find the job work factory registered under the name `Name`.
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

end AWA.Jobs.Modules;
