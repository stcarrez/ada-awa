-----------------------------------------------------------------------
--  atlas -- atlas applications
-----------------------------------------------------------------------
--  Copyright (C) 2011 unknown
--  Written by unknown (unknown@company.com)
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

with Ada.IO_Exceptions;

with Util.Log.Loggers;
with Util.Properties;

with ASF.Applications;
with ASF.Applications.Main;
with ASF.Applications.Main.Configs;


--  with Atlas.XXX.Module;
package body Atlas.Applications is

   use AWA.Applications;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Atlas");

   --  ------------------------------
   --  Initialize the application:
   --  <ul>
   --     <li>Register the servlets and filters.
   --     <li>Register the application modules.
   --     <li>Define the servlet and filter mappings.
   --  </ul>
   --  ------------------------------
   procedure Initialize (App : in Application_Access) is
      Fact  : ASF.Applications.Main.Application_Factory;
      C     : ASF.Applications.Config;
   begin
      App.Self := App;
      begin
         C.Load_Properties ("atlas.properties");
         Util.Log.Loggers.Initialize (Util.Properties.Manager (C));

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Cannot read application configuration file {0}", CONFIG_PATH);
      end;
      App.Initialize (C, Fact);

      App.Set_Global ("contextPath", CONTEXT_PATH);
      ASF.Applications.Main.Configs.Read_Configuration (App.all, "web/WEB-INF/web.xml");
   end Initialize;

   --  ------------------------------
   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   --  ------------------------------
   overriding
   procedure Initialize_Servlets (App : in out Application) is
   begin
      Log.Info ("Initializing application servlets...");

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
      App.Add_Filter (Name => "dump", Filter => App.Dump'Access);
      App.Add_Filter (Name => "measures", Filter => App.Measures'Access);
      App.Add_Filter (Name => "service", Filter => App.Service_Filter'Access);
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

      Register (App    => App.Self.all'Access,
                Name   => AWA.Users.Module.NAME,
                URI    => "user",
                Module => App.User_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => "workspaces",
                URI    => "workspaces",
                Module => App.Workspace_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Mail.Module.NAME,
                URI    => "mail",
                Module => App.Mail_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Storages.Modules.NAME,
                URI    => "storages",
                Module => App.Storage_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Blogs.Module.NAME,
                URI    => "blogs",
                Module => App.Blog_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Comments.Module.NAME,
                URI    => "comments",
                Module => App.Comment_Module'Access);
   end Initialize_Modules;

end Atlas.Applications;
