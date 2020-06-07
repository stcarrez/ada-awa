-----------------------------------------------------------------------
--  awa_test_app -
--  Copyright (C) 2020 Stephane Carrez
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
with Util.Log.Loggers;

package body AWA_Test_App is

   use AWA.Applications;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Adafr");

   --  ------------------------------
   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   --  ------------------------------
   overriding
   procedure Initialize_Servlets (App : in out Application) is
   begin
      Log.Info ("Initializing application servlets...");

      App.Self := App'Unchecked_Access;
      App.Set_Global ("contextPath", CONTEXT_PATH);
      AWA.Applications.Application (App).Initialize_Servlets;
      App.Add_Servlet (Name => "faces", Server => App.Self.Faces'Access);
      App.Add_Servlet (Name => "files", Server => App.Self.Files'Access);
      App.Add_Servlet (Name => "ajax", Server => App.Self.Ajax'Access);
      App.Add_Servlet (Name => "measures", Server => App.Self.Measures'Access);
      App.Add_Servlet (Name => "auth", Server => App.Self.Auth'Access);
      App.Add_Servlet (Name => "verify-auth", Server => App.Self.Verify_Auth'Access);
   end Initialize_Servlets;

   --  ------------------------------
   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   --  ------------------------------
   overriding
   procedure Initialize_Filters (App : in out Application) is
   begin
      Log.Info ("Initializing application filters...");

      AWA.Applications.Application (App).Initialize_Filters;
      App.Add_Filter (Name => "dump", Filter => App.Self.Dump'Access);
      App.Add_Filter (Name => "measures", Filter => App.Self.Measures'Access);
      App.Add_Filter (Name => "service", Filter => App.Self.Service_Filter'Access);
      App.Add_Filter (Name => "no-cache", Filter => App.Self.No_Cache'Access);
   end Initialize_Filters;

   --  ------------------------------
   --  Initialize the AWA modules provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the modules used by the application.
   --  ------------------------------
   overriding
   procedure Initialize_Modules (App : in out Application) is
   begin
      Log.Info ("Initializing application modules...");

      App.Add_Converter (Name      => "smartDateConverter",
                         Converter => App.Self.Rel_Date_Converter'Access);
      App.Add_Converter (Name      => "sizeConverter",
                         Converter => App.Self.Size_Converter'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Users.Modules.NAME,
                URI    => "user",
                Module => App.User_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Workspaces.Modules.NAME,
                URI    => "workspaces",
                Module => App.Workspace_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Mail.Modules.NAME,
                URI    => "mail",
                Module => App.Mail_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Comments.Modules.NAME,
                URI    => "comments",
                Module => App.Comment_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Tags.Modules.NAME,
                URI    => "tags",
                Module => App.Tag_Module'Access);
      Register (App    => App.Self.all'Access,
                Name   => AWA.Jobs.Modules.NAME,
                URI    => "jobs",
                Module => App.Job_Module'Access);
      Register (App    => App.Self.all'Access,
                Name   => AWA.Storages.Modules.NAME,
                URI    => "storages",
                Module => App.Storage_Module'Access);
      Register (App    => App.Self.all'Access,
                Name   => AWA.Images.Modules.NAME,
                URI    => "images",
                Module => App.Image_Module'Access);
      Register (App    => App.Self.all'Access,
                Name   => AWA.Counters.Modules.NAME,
                URI    => "counters",
                Module => App.Counter_Module'Access);
      Register (App    => App.Self.all'Access,
                Name   => AWA.Blogs.Modules.NAME,
                URI    => "blogs",
                Module => App.Blog_Module'Access);
      Register (App    => App.Self.all'Access,
                Name   => AWA.Wikis.Modules.NAME,
                URI    => "wikis",
                Module => App.Wiki_Module'Access);
      Register (App    => App.Self.all'Access,
                Name   => AWA.Wikis.Previews.NAME,
                URI    => "wikis-preview",
                Module => App.Preview_Module'Access);
   end Initialize_Modules;

end AWA_Test_App;
