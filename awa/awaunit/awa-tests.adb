-----------------------------------------------------------------------
--  AWA tests - AWA Tests Framework
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with ASF.Server.Tests;
with ASF.Server.Web;

with ASF.Tests;

--  with AWA.Applications;
with AWA.Applications.Factory;
with AWA.Services.Filters;
with AWA.Services.Contexts;
package body AWA.Tests is

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
      ASF.Server.Tests.Set_Context (Application.all'Access);
   end Set_Up;

   procedure Initialize (Props : in Util.Properties.Manager) is
   begin
      Initialize (null, Props, True);
   end Initialize;

   --  ------------------------------
   --  Called when the testsuite execution has finished.
   --  ------------------------------
   procedure Finish (Status : in Util.XUnit.Status) is
      pragma Unreferenced (Status);

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => AWA.Applications.Application'Class,
                                        Name   => AWA.Applications.Application_Access);

   begin
      if Application_Created then
         Free (Application);
      end if;
   end Finish;

   --  ------------------------------
   --  Initialize the AWA test framework mockup.
   --  ------------------------------
   procedure Initialize (App         : in AWA.Applications.Application_Access;
                         Props       : in Util.Properties.Manager;
                         Add_Modules : in Boolean) is
      use AWA.Applications;
   begin
      --  Create the application unless it is specified as argument.
      --  Install a shutdown hook to delete the application when the primary task exits.
      --  This allows to stop the event threads if any.
      if App = null then
         Application_Created := True;
         Application := new AWA.Applications.Application;
         Ada.Task_Termination.Set_Specific_Handler (Ada.Task_Identification.Current_Task,
                                                    Shutdown.Termination'Access);
      else
         Application := App;
      end if;

      ASF.Tests.Initialize (Props, Application.all'Access, Factory);
      Application.Add_Filter ("service", Service_Filter'Access);
      Application.Add_Filter_Mapping (Name => "service", Pattern => "*.html");
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
      ASF.Server.Tests.Set_Context (Application.all'Access);
   end Set_Application_Context;

end AWA.Tests;
