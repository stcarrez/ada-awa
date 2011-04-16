-----------------------------------------------------------------------
--  AWA tests - AWA Tests Framework
--  Copyright (C) 2011 Stephane Carrez
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

with Ada.Strings.Unbounded;

with Util.Tests;
with Util.Files;

with ASF.Servlets.Faces;
with ASF.Servlets.Files;
with ASF.Servlets.Measures;
with ASF.Responses;
with ASF.Responses.Tools;

with AWA.Applications;
with ASF.Applications.Main;
with ASF.Filters.Dump;

with ADO.Drivers;

with ASF.Tests;
package body AWA.Tests is

   use Ada.Strings.Unbounded;

   CONTEXT_PATH : constant String := "/awa";

   Server   : access ASF.Server.Container;

   App      : AWA.Applications.Application_Access := null;
   Fact     : ASF.Applications.Main.Application_Factory;
   Faces    : aliased ASF.Servlets.Faces.Faces_Servlet;
   Files    : aliased ASF.Servlets.Files.File_Servlet;
   Dump     : aliased ASF.Filters.Dump.Dump_Filter;
   Measures : aliased ASF.Servlets.Measures.Measure_Servlet;

   --  ------------------------------
   --  Initialize the awa test framework mockup.
   --  ------------------------------
   procedure Initialize (Props : in Util.Properties.Manager) is
      C        : ASF.Applications.Config;
      Database : constant String := Props.Get ("test.database");
   begin
      ADO.Drivers.Initialize;

      App := new AWA.Applications.Application;

      Server := new ASF.Server.Container;
      Server.Register_Application (CONTEXT_PATH, App.all'Access);

      C.Copy (Props);
      C.Set ("database", Database);
      App.Initialize (C, Fact);
      App.Register ("layoutMsg", "layout");
      App.Set_Global ("contextPath", "/awa");

      --  Register the servlets and filters
      App.Add_Servlet (Name => "faces", Server => Faces'Access);
      App.Add_Servlet (Name => "files", Server => Files'Access);
      App.Add_Servlet (Name => "measures", Server => Measures'Access);
      App.Add_Filter (Name => "dump", Filter => Dump'Access);
      App.Add_Filter (Name => "measures", Filter => Measures'Access);

      --  Define servlet mappings
      App.Add_Mapping (Name => "faces", Pattern => "*.html");
      App.Add_Mapping (Name => "files", Pattern => "*.css");
      App.Add_Mapping (Name => "files", Pattern => "*.js");
      App.Add_Mapping (Name => "measures", Pattern => "stats.xml");

      App.Add_Filter_Mapping (Name => "measures", Pattern => "*");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.css");
   end Initialize;

   --  ------------------------------
   --  Get the server
   --  ------------------------------
   function Get_Server return access ASF.Server.Container is
   begin
      return Server;
   end Get_Server;

   --  ------------------------------
   --  Get the test application.
   --  ------------------------------
   function Get_Application return AWA.Applications.Application_Access is
   begin
      return App;
   end Get_Application;

end AWA.Tests;
