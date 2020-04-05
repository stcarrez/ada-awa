-----------------------------------------------------------------------
--  Util testsuite - Util Testsuite
--  Copyright (C) 2009 - 2020 Stephane Carrez
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
with AWA.Blogs.Modules.Tests;
with AWA.Blogs.Tests;
with AWA.Helpers.Selectors.Tests;
with AWA.Storages.Services.Tests;
with AWA.Events.Services.Tests;
with AWA.Mail.Clients.Tests;
with AWA.Mail.Modules.Tests;
with AWA.Images.Modules.Tests;
with AWA.Votes.Modules.Tests;
with AWA.Tags.Modules.Tests;
with AWA.Questions.Modules.Tests;
with AWA.Questions.Tests;
with AWA.Counters.Modules.Tests;
with AWA.Workspaces.Tests;
with AWA.Modules.Tests;

with ASF.Converters.Dates;
with ASF.Converters.Sizes;

with AWA.Users.Modules;
with AWA.Mail.Modules;
with AWA.Blogs.Modules;
with AWA.Workspaces.Modules;
with AWA.Storages.Modules;
with AWA.Images.Modules;
with AWA.Questions.Modules;
with AWA.Votes.Modules;
with AWA.Tags.Modules;
with AWA.Changelogs.Modules;
with AWA.Counters.Modules;

with AWA.Converters.Dates;
with AWA.Tests;
with AWA.Services.Contexts;
with AWA.Jobs.Services.Tests;
with AWA.Jobs.Modules.Tests;
with AWA.Settings.Modules.Tests;
with AWA.Comments.Modules.Tests;
with AWA.Changelogs.Modules.Tests;
with AWA.Wikis.Modules.Tests;
with AWA.Wikis.Tests;
with AWA.Commands.Tests;

with ADO.Drivers;
with Servlet.Server;
with Security.Auth;
with Security.Auth.Fake;
package body AWA.Testsuite is

   function OAuth_Provider_Factory (Name : in String) return Security.Auth.Manager_Access;

   Users          : aliased AWA.Users.Modules.User_Module;

   Workspaces     : aliased AWA.Workspaces.Modules.Workspace_Module;

   Mail           : aliased AWA.Mail.Modules.Mail_Module;

   Jobs           : aliased AWA.Jobs.Modules.Job_Module;

   Comments       : aliased AWA.Comments.Modules.Comment_Module;

   Blogs          : aliased AWA.Blogs.Modules.Blog_Module;

   Storages       : aliased AWA.Storages.Modules.Storage_Module;

   Images         : aliased AWA.Images.Modules.Image_Module;

   Questions      : aliased AWA.Questions.Modules.Question_Module;

   Votes          : aliased AWA.Votes.Modules.Vote_Module;

   Tags           : aliased AWA.Tags.Modules.Tag_Module;

   Settings       : aliased AWA.Settings.Modules.Setting_Module;

   Changelogs     : aliased AWA.Changelogs.Modules.Changelog_Module;

   Wikis          : aliased AWA.Wikis.Modules.Wiki_Module;

   Counters       : aliased AWA.Counters.Modules.Counter_Module;

   Date_Converter : aliased ASF.Converters.Dates.Date_Converter;

   Rel_Date_Converter : aliased AWA.Converters.Dates.Relative_Date_Converter;

   Size_Converter : aliased ASF.Converters.Sizes.Size_Converter;

   Tests : aliased Util.Tests.Test_Suite;

   function OAuth_Provider_Factory (Name : in String) return Security.Auth.Manager_Access is
      pragma Unreferenced (Name);
   begin
      return new Security.Auth.Fake.Manager;
   end OAuth_Provider_Factory;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      Security.Auth.Set_Default_Factory (OAuth_Provider_Factory'Access);
      AWA.Commands.Tests.Add_Tests (Ret);
      AWA.Jobs.Modules.Tests.Add_Tests (Ret);
      AWA.Jobs.Services.Tests.Add_Tests (Ret);
      AWA.Settings.Modules.Tests.Add_Tests (Ret);
      AWA.Comments.Modules.Tests.Add_Tests (Ret);
      AWA.Blogs.Modules.Tests.Add_Tests (Ret);
      AWA.Blogs.Tests.Add_Tests (Ret);
      AWA.Storages.Services.Tests.Add_Tests (Ret);
      AWA.Images.Modules.Tests.Add_Tests (Ret);
      AWA.Changelogs.Modules.Tests.Add_Tests (Ret);
      AWA.Votes.Modules.Tests.Add_Tests (Ret);
      AWA.Tags.Modules.Tests.Add_Tests (Ret);
      AWA.Questions.Modules.Tests.Add_Tests (Ret);
      AWA.Questions.Tests.Add_Tests (Ret);
      AWA.Wikis.Modules.Tests.Add_Tests (Ret);
      AWA.Wikis.Tests.Add_Tests (Ret);
      AWA.Events.Services.Tests.Add_Tests (Ret);
      AWA.Mail.Clients.Tests.Add_Tests (Ret);
      AWA.Mail.Modules.Tests.Add_Tests (Ret);
      AWA.Workspaces.Tests.Add_Tests (Ret);
      AWA.Users.Services.Tests.Add_Tests (Ret);
      AWA.Users.Tests.Add_Tests (Ret);
      AWA.Counters.Modules.Tests.Add_Tests (Ret);
      AWA.Helpers.Selectors.Tests.Add_Tests (Ret);
      AWA.Modules.Tests.Add_Tests (Ret);
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
      ADO.Drivers.Initialize;
      AWA.Tests.Initialize (App, Props, Add_Modules);

      if Add_Modules then
         declare
            Application : constant Applications.Application_Access := AWA.Tests.Get_Application;
            Ctx         : AWA.Services.Contexts.Service_Context;
            Users  : constant AWA.Users.Modules.User_Module_Access := AWA.Testsuite.Users'Access;
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
                      Name   => AWA.Counters.Modules.NAME,
                      URI    => "counters",
                      Module => Counters'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Comments.Modules.NAME,
                      URI    => "comments",
                      Module => Comments'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Storages.Modules.NAME,
                      URI    => "storages",
                      Module => Storages'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Jobs.Modules.NAME,
                      URI    => "jobs",
                      Module => Jobs'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Images.Modules.NAME,
                      URI    => "images",
                      Module => Images'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Settings.Modules.NAME,
                      URI    => "settings",
                      Module => Settings'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Votes.Modules.NAME,
                      URI    => "votes",
                      Module => Votes'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Changelogs.Modules.NAME,
                      URI    => "changelogs",
                      Module => Changelogs'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Tags.Modules.NAME,
                      URI    => "tags",
                      Module => Tags'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Blogs.Modules.NAME,
                      URI    => "blogs",
                      Module => Blogs'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Questions.Modules.NAME,
                      URI    => "questions",
                      Module => Questions'Access);

            Register (App    => Application.all'Access,
                      Name   => AWA.Wikis.Modules.NAME,
                      URI    => "wikis",
                      Module => Wikis'Access);

            Application.Add_Converter (Name      => "dateConverter",
                                       Converter => Date_Converter'Access);
            Application.Add_Converter (Name      => "smartDateConverter",
                                       Converter => Rel_Date_Converter'Access);
            Application.Add_Converter (Name      => "sizeConverter",
                                       Converter => Size_Converter'Access);
            Application.Start;
--              if Props.Exists ("test.server") then
--                 declare
--                    WS : ASF.Server.Web.AWS_Container;
--                 begin
--                    Application.Add_Converter (Name      => "dateConverter",
--                                               Converter => Date_Converter'Access);
--                    Application.Add_Converter (Name      => "smartDateConverter",
--                                               Converter => Rel_Date_Converter'Access);
--
--                    WS.Register_Application ("/asfunit", Application.all'Access);
--
--                    WS.Start;
--                    delay 6000.0;
--                 end;
--              end if;
            Servlet.Server.Set_Context (Application.all'Access);
         end;
      end if;
   end Initialize;

end AWA.Testsuite;
