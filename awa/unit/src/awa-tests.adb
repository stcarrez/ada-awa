-----------------------------------------------------------------------
--  AWA tests - AWA Tests Framework
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018 Stephane Carrez
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
with Ada.Task_Termination;
with Ada.Task_Identification;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Util.Log.Loggers;

with ASF.Tests;

with Servlet.Server;
with Servlet.Tests;
with ASF.Applications;
with AWA.Applications.Factory;
with AWA.Tests.Helpers.Users;
package body AWA.Tests is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Tests");

   protected Shutdown is
      procedure Termination (Cause : in Ada.Task_Termination.Cause_Of_Termination;
                             Id    : in Ada.Task_Identification.Task_Id;
                             Ex    : in Ada.Exceptions.Exception_Occurrence);
   end Shutdown;

   Application_Created : Boolean := False;
   Application    : AWA.Applications.Application_Access := null;

   Factory        : AWA.Applications.Factory.Application_Factory;

   Service_Filter : aliased AWA.Services.Filters.Service_Filter;

   protected body Shutdown is
      procedure Termination (Cause : in Ada.Task_Termination.Cause_Of_Termination;
                             Id    : in Ada.Task_Identification.Task_Id;
                             Ex    : in Ada.Exceptions.Exception_Occurrence) is
         pragma Unreferenced (Cause, Id, Ex);

         procedure Free is
            new Ada.Unchecked_Deallocation (Object => AWA.Applications.Application'Class,
                                            Name   => AWA.Applications.Application_Access);
      begin
         Free (Application);
      end Termination;
   end Shutdown;

   --  ------------------------------
   --  Setup the service context before executing the test.
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Servlet.Server.Set_Context (Application.all'Access);
      null;
   end Set_Up;

   --  ------------------------------
   --  Cleanup after the test execution.
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AWA.Tests.Helpers.Users.Tear_Down;
   end Tear_Down;

   procedure Initialize (Props : in Util.Properties.Manager) is
   begin
      Initialize (null, Props, True);
   end Initialize;

   --  ------------------------------
   --  Called when the testsuite execution has finished.
   --  ------------------------------
   procedure Finish (Status : in Util.XUnit.Status) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => AWA.Applications.Application'Class,
                                        Name   => AWA.Applications.Application_Access);

   begin
      if Application_Created then
         Application.Close;
         Free (Application);
      end if;
      ASF.Tests.Finish (Status);
   end Finish;

   --  ------------------------------
   --  Initialize the AWA test framework mockup.
   --  ------------------------------
   procedure Initialize (App         : in AWA.Applications.Application_Access;
                         Props       : in Util.Properties.Manager;
                         Add_Modules : in Boolean) is
      pragma Unreferenced (Add_Modules);
      use AWA.Applications;
      C        : ASF.Applications.Config;
      Empty    : Util.Properties.Manager;
   begin
      --  Create the application unless it is specified as argument.
      --  Install a shutdown hook to delete the application when the primary task exits.
      --  This allows to stop the event threads if any.
      if App = null then
         Application_Created := True;
         Application := new Test_Application;
         Ada.Task_Termination.Set_Specific_Handler (Ada.Task_Identification.Current_Task,
                                                    Shutdown.Termination'Access);
      else
         Application := App;
      end if;
      C.Copy (Props);
      Application.Initialize (C, Factory);
      Application.Set_Global ("contextPath", "/asfunit");
      Servlet.Tests.Initialize (Empty, "/asfunit", Application.all'Access);

      Application.Add_Filter ("service", Service_Filter'Access);
   end Initialize;

   --  ------------------------------
   --  Get the test application.
   --  ------------------------------
   function Get_Application return AWA.Applications.Application_Access is
   begin
      return Application;
   end Get_Application;

   --  ------------------------------
   --  Set the application context to simulate a web request context.
   --  ------------------------------
   procedure Set_Application_Context is
   begin
      Servlet.Server.Set_Context (Application.all'Access);
   end Set_Application_Context;

   --  ------------------------------
   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   --  ------------------------------
   overriding
   procedure Initialize_Servlets (App : in out Test_Application) is
   begin
      Log.Info ("Initializing application servlets...");

      AWA.Applications.Application (App).Initialize_Servlets;
      App.Add_Servlet (Name => "faces", Server => App.Faces'Unchecked_Access);
      App.Add_Servlet (Name => "files", Server => App.Files'Unchecked_Access);
      App.Add_Servlet (Name => "ajax", Server => App.Ajax'Unchecked_Access);
      App.Add_Servlet (Name => "measures", Server => App.Measures'Unchecked_Access);
   end Initialize_Servlets;

   --  ------------------------------
   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   --  ------------------------------
   overriding
   procedure Initialize_Filters (App : in out Test_Application) is
   begin
      Log.Info ("Initializing application filters...");

      AWA.Applications.Application (App).Initialize_Filters;
      App.Add_Filter (Name => "dump", Filter => App.Dump'Unchecked_Access);
      App.Add_Filter (Name => "measures", Filter => App.Measures'Unchecked_Access);
      App.Add_Filter (Name => "service", Filter => App.Service_Filter'Unchecked_Access);
   end Initialize_Filters;

end AWA.Tests;
