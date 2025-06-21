-----------------------------------------------------------------------
--  AWA tests - AWA Tests Framework
--  Copyright (C) 2011, 2012, 2014, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Applications;
with AWA.Services.Filters;

with Util.Properties;
with Util.Tests;
with Util.XUnit;

with ASF.Filters.Dump;
with ASF.Servlets.Faces;
with Servlet.Core.Files;
with ASF.Servlets.Ajax;
with Servlet.Core.Measures;
package AWA.Tests is

   type Test is abstract new Util.Tests.Test with null record;

   --  Setup the service context before executing the test.
   overriding
   procedure Set_Up (T : in out Test);

   --  Cleanup after the test execution.
   overriding
   procedure Tear_Down (T : in out Test);

   --  Initialize the AWA test framework mockup.
   procedure Initialize (App         : in AWA.Applications.Application_Access;
                         Props       : in Util.Properties.Manager;
                         Add_Modules : in Boolean);

   --  Called when the testsuite execution has finished.
   procedure Finish (Status : in Util.XUnit.Status);

   procedure Initialize (Props : in Util.Properties.Manager);

   --  Get the test application.
   function Get_Application return AWA.Applications.Application_Access;

   --  Set the application context to simulate a web request context.
   procedure Set_Application_Context;

   type Test_Application is new AWA.Applications.Application with record
      Self              : AWA.Applications.Application_Access;

      --  Application servlets and filters (add new servlet and filter instances here).
      Faces             : aliased ASF.Servlets.Faces.Faces_Servlet;
      Ajax              : aliased ASF.Servlets.Ajax.Ajax_Servlet;
      Files             : aliased Servlet.Core.Files.File_Servlet;
      Dump              : aliased ASF.Filters.Dump.Dump_Filter;
      Service_Filter    : aliased AWA.Services.Filters.Service_Filter;
      Measures          : aliased Servlet.Core.Measures.Measure_Servlet;
   end record;

   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   overriding
   procedure Initialize_Servlets (App : in out Test_Application);

   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   overriding
   procedure Initialize_Filters (App : in out Test_Application);

end AWA.Tests;
