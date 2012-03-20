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
with ASF.Converters.Dates;

with ASF.Tests;
with Ada.Text_IO;

with AWA.Users.Module;
with AWA.Mail.Module;
with AWA.Blogs.Module;
with AWA.Workspaces.Module;

--  with AWA.Applications;
with AWA.Applications.Factory;
with AWA.Services.Filters;
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

   Users          : aliased AWA.Users.Module.User_Module;

   Workspaces     : aliased AWA.Workspaces.Module.Workspace_Module;

   Mail           : aliased AWA.Mail.Module.Mail_Module;

   Blogs          : aliased AWA.Blogs.Module.Blog_Module;

   Date_Converter : aliased ASF.Converters.Dates.Date_Converter;

   protected body Shutdown is
      procedure Termination (Cause : in Ada.Task_Termination.Cause_Of_Termination;
                             Id    : in Ada.Task_Identification.Task_Id;
                             Ex    : in Ada.Exceptions.Exception_Occurrence) is
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

      if Add_Modules then
         declare
            Users : constant AWA.Users.Module.User_Module_Access := AWA.Tests.Users'Access;
         begin
            Register (App    => Application.all'Access,
                      Name   => AWA.Users.Module.NAME,
                      URI    => "user",
                      Module => Users.all'Access);

            Register (App    => Application.all'Access,
                      Name   => "mail",
                      URI    => "mail",
                      Module => Mail'Access);

            Register (App    => Application.all'Access,
                      Name   => "workspaces",
                      URI    => "workspaces",
                      Module => Workspaces'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Blogs.Module.NAME,
                      URI    => "blogs",
                      Module => Blogs'Access);

            if Props.Exists ("test.server") then
               declare
                  WS : ASF.Server.Web.AWS_Container;
               begin
                  Application.Add_Converter (Name      => "dateConverter",
                                             Converter => Date_Converter'Access);

                  WS.Register_Application ("/asfunit", Application.all'Access);

                  WS.Start;
                  delay 6000.0;
               end;
            end if;
            ASF.Server.Tests.Set_Context (Application.all'Access);
         end;
      end if;
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
