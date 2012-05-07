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
with GNAT.MD5;

with Util.Log.Loggers;
with Util.Properties;
with Util.Strings.Transforms;

with EL.Functions;

with ASF.Applications;
with ASF.Applications.Main;
with ASF.Applications.Main.Configs;

with AWA.Applications.Configs;
with AWA.Applications.Factory;


--  with Atlas.XXX.Module;
package body Atlas.Applications is

   use AWA.Applications;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Atlas");

   --  ------------------------------
   --  Given an Email address, return the Gravatar link to the user image.
   --  (See http://en.gravatar.com/site/implement/hash/ and
   --  http://en.gravatar.com/site/implement/images/)
   --  ------------------------------
   function Get_Gravatar_Link (Email : in String) return String is
      E : constant String := Util.Strings.Transforms.To_Lower_Case (Email);
      C : constant GNAT.MD5.Message_Digest := GNAT.MD5.Digest (E);
   begin
      return "http://www.gravatar.com/avatar/" & C;
   end Get_Gravatar_Link;

   --  ------------------------------
   --  EL function to convert an Email address to a Gravatar image.
   --  ------------------------------
   function To_Gravatar_Link (Email : in Util.Beans.Objects.Object)
                              return Util.Beans.Objects.Object is
      Link : constant String := Get_Gravatar_Link (Util.Beans.Objects.To_String (Email));
   begin
      return Util.Beans.Objects.To_Object (Link);
   end To_Gravatar_Link;

   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
   begin
      Mapper.Set_Function (Name      => "gravatar",
                           Namespace => ATLAS_NS_URI,
                           Func      => To_Gravatar_Link'Access);
   end Set_Functions;

   --  ------------------------------
   --  Initialize the application:
   --  <ul>
   --     <li>Register the servlets and filters.
   --     <li>Register the application modules.
   --     <li>Define the servlet and filter mappings.
   --  </ul>
   --  ------------------------------
   procedure Initialize (App : in Application_Access) is
      Fact  : AWA.Applications.Factory.Application_Factory;
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
   end Initialize;

   --  ------------------------------
   --  Initialize the ASF components provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the component factories used by the application.
   --  ------------------------------
   overriding
   procedure Initialize_Components (App : in out Application) is
      procedure Register is
         new ASF.Applications.Main.Register_Functions (Set_Functions);
   begin
      Register (App);
      AWA.Applications.Application (App).Initialize_Components;
   end Initialize_Components;

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
      ASF.Applications.Main.Configs.Read_Configuration (App, "web/WEB-INF/web.xml");

      Register (App    => App.Self.all'Access,
                Name   => AWA.Users.Modules.NAME,
                URI    => "user",
                Module => App.User_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => "workspaces",
                URI    => "workspaces",
                Module => App.Workspace_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Mail.Modules.NAME,
                URI    => "mail",
                Module => App.Mail_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Storages.Modules.NAME,
                URI    => "storages",
                Module => App.Storage_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Blogs.Modules.NAME,
                URI    => "blogs",
                Module => App.Blog_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => AWA.Comments.Modules.NAME,
                URI    => "comments",
                Module => App.Comment_Module'Access);

      Register (App    => App.Self.all'Access,
                Name   => Atlas.Microblog.Modules.NAME,
                URI    => "microblog",
                Module => App.Microblog_Module'Access);
   end Initialize_Modules;

end Atlas.Applications;
