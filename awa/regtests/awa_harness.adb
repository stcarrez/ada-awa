-----------------------------------------------------------------------
--  AWA - Unit tests
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with AWA.Testsuite;
with AWA.Users.Module;
with AWA.Mail.Module;
with AWA.Applications;

with ASF.Applications.Main;
with ASF.Server.Web;
with ASF.Navigations;

with Util.Tests;
with Util.Properties;
with AWA.Tests;
procedure AWA_Harness is

   use AWA.Applications;

--     App   : AWA.Applications.Application_Access := new AWA.Applications.Application;
--     Fact  : ASF.Applications.Main.Application_Factory;
--     Faces : aliased ASF.Servlets.Faces.Faces_Servlet;
--     Files : aliased ASF.Servlets.Files.File_Servlet;
--     Dump  : aliased ASF.Filters.Dump.Dump_Filter;

   procedure Initialize (Props : in Util.Properties.Manager);

   procedure Harness is new Util.Tests.Harness (AWA.Testsuite.Suite,
                                               Initialize);

   --  ------------------------------
   --  Initialization procedure: setup the database
   --  ------------------------------
   procedure Initialize (Props : in Util.Properties.Manager) is
   begin
      AWA.Tests.Initialize (Props);

      declare
         App : constant AWA.Applications.Application_Access := AWA.Tests.Get_Application;
      begin

         Register (App    => App.all'Access,
                   Name   => "users", URI => "user",
                   Module => AWA.Users.Module.Instance.all'Access);

         Register (App    => App.all'Access,
                   Name   => "mail", URI => "mail",
                   Module => AWA.Mail.Module.Instance.all'Access);

         declare
            Nav : ASF.Navigations.Navigation_Handler_Access := App.Get_Navigation_Handler;
         begin
            Nav.Add_Navigation_Case (From    => "/users/login.xhtml",
                                     To      => "/users/main.xhtml",
                                     Outcome => "success");
            Nav.Add_Navigation_Case (From    => "/users/register.xhtml",
                                     To      => "/users/registration-sent.xhtml",
                                     Outcome => "success");
         end;
         if Props.Exists ("test.server") then
            declare
               WS : ASF.Server.Web.AWS_Container;
            begin
               WS.Register_Application ("/awa", App.all'Access);

               WS.Start;
               delay 600.0;
            end;
         end if;
      end;
   end Initialize;

begin
   Harness ("awa-tests.xml");
end AWA_Harness;
