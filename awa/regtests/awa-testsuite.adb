-----------------------------------------------------------------------
--  Util testsuite - Util Testsuite
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with AWA.Users.Services.Tests;
with AWA.Users.Tests;
with AWA.Blogs.Services.Tests;
with AWA.Wikis.Parsers.Tests;
with AWA.Helpers.Selectors.Tests;
with AWA.Storages.Services.Tests;
with AWA.Events.Services.Tests;
with AWA.Mail.Clients.Tests;
with AWA.Mail.Modules.Tests;

with ASF.Converters.Dates;

with AWA.Users.Modules;
with AWA.Mail.Modules;
with AWA.Blogs.Modules;
with AWA.Workspaces.Modules;
with AWA.Storages.Modules;

with AWA.Converters.Dates;
with AWA.Tests;
with AWA.Services.Contexts;
with AWA.Jobs.Services.Tests;
with AWA.Jobs.Modules.Tests;

with ASF.Server.Web;
with ASF.Servlets.Faces;
with ASF.Server.Tests;
package body AWA.Testsuite is
   Users          : aliased AWA.Users.Modules.User_Module;

   Workspaces     : aliased AWA.Workspaces.Modules.Workspace_Module;

   Mail           : aliased AWA.Mail.Modules.Mail_Module;

   Jobs           : aliased AWA.Jobs.Modules.Job_Module;

   Blogs          : aliased AWA.Blogs.Modules.Blog_Module;

   Storages       : aliased AWA.Storages.Modules.Storage_Module;

   Date_Converter : aliased ASF.Converters.Dates.Date_Converter;

   Rel_Date_Converter : aliased AWA.Converters.Dates.Relative_Date_Converter;
   Faces              : aliased ASF.Servlets.Faces.Faces_Servlet;

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      AWA.Events.Services.Tests.Add_Tests (Ret);
      AWA.Mail.Clients.Tests.Add_Tests (Ret);
      AWA.Mail.Modules.Tests.Add_Tests (Ret);
      AWA.Users.Services.Tests.Add_Tests (Ret);
      AWA.Users.Tests.Add_Tests (Ret);
      AWA.Wikis.Parsers.Tests.Add_Tests (Ret);
      AWA.Helpers.Selectors.Tests.Add_Tests (Ret);
      AWA.Jobs.Modules.Tests.Add_Tests (Ret);
      AWA.Jobs.Services.Tests.Add_Tests (Ret);
      AWA.Blogs.Services.Tests.Add_Tests (Ret);
      AWA.Storages.Services.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

   procedure Initialize (Props : in Util.Properties.Manager) is
   begin
      Initialize (null, Props, True);
   end Initialize;

   --  ------------------------------
   --  Initialize the AWA test framework mockup.
   --  ------------------------------
   procedure Initialize (App         : in AWA.Applications.Application_Access;
                         Props       : in Util.Properties.Manager;
                         Add_Modules : in Boolean) is
      use AWA.Applications;
   begin
      AWA.Tests.Initialize (App, Props, Add_Modules);

      if Add_Modules then
         declare
            Application : AWA.Applications.Application_Access := AWA.Tests.Get_Application;
            Ctx    : AWA.Services.Contexts.Service_Context;
            Users : constant AWA.Users.Modules.User_Module_Access := AWA.Testsuite.Users'Access;
         begin
            Ctx.Set_Context (Application, null);
            Register (App    => Application.all'Access,
                      Name   => AWA.Users.Modules.NAME,
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
                      Name   => AWA.Storages.Modules.NAME,
                      URI    => "storages",
                      Module => Storages'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Jobs.Modules.NAME,
                      URI    => "jobs",
                      Module => Jobs'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Blogs.Modules.NAME,
                      URI    => "blogs",
                      Module => Blogs'Access);

            if Props.Exists ("test.server") then
               declare
                  WS : ASF.Server.Web.AWS_Container;
               begin
                  Application.Add_Converter (Name      => "dateConverter",
                                             Converter => Date_Converter'Access);
                  Application.Add_Converter (Name      => "smartDateConverter",
                                             Converter => Rel_Date_Converter'Access);

                  WS.Register_Application ("/asfunit", Application.all'Access);

                  WS.Start;
                  delay 6000.0;
               end;
            end if;
            ASF.Server.Tests.Set_Context (Application.all'Access);
         end;
      end if;
   end Initialize;

end AWA.Testsuite;
