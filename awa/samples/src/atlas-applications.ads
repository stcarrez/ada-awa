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
with ASF.Servlets.Faces;
with ASF.Servlets.Files;
with ASF.Servlets.Ajax;
with ASF.Filters.Dump;
with ASF.Servlets.Measures;

with Security.Openid.Servlets;

with AWA.Users.Servlets;
with AWA.Users.Module;
with AWA.Mail.Module;
with AWA.Comments.Module;
with AWA.Blogs.Module;
with AWA.Storages.Modules;
with AWA.Applications;
with AWA.Workspaces.Module;
with AWA.Services.Filters;

with Atlas.Microblog.Modules;
package Atlas.Applications is

   CONFIG_PATH  : constant String := "/atlas";
   CONTEXT_PATH : constant String := "/atlas";

   type Application is new AWA.Applications.Application with private;
   type Application_Access is access all Application'Class;

   --  Initialize the application.
   procedure Initialize (App : in Application_Access);

   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   overriding
   procedure Initialize_Servlets (App : in out Application);

   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   overriding
   procedure Initialize_Filters (App : in out Application);

   --  Initialize the AWA modules provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the modules used by the application.
   overriding
   procedure Initialize_Modules (App : in out Application);

private

   type Application is new AWA.Applications.Application with record
      Self              : Application_Access;
      --  Application servlets and filters (add new servlet and filter instances here).
      Faces             : aliased ASF.Servlets.Faces.Faces_Servlet;
      Ajax              : aliased ASF.Servlets.Ajax.Ajax_Servlet;
      Files             : aliased ASF.Servlets.Files.File_Servlet;
      Dump              : aliased ASF.Filters.Dump.Dump_Filter;
      Service_Filter    : aliased AWA.Services.Filters.Service_Filter;
      Measures          : aliased ASF.Servlets.Measures.Measure_Servlet;

      --  Authentication servlet and filter.
      Auth              : aliased Security.Openid.Servlets.Request_Auth_Servlet;
      Verify_Auth       : aliased AWA.Users.Servlets.Verify_Auth_Servlet;

      --  The application modules.
      User_Module       : aliased AWA.Users.Module.User_Module;
      Workspace_Module  : aliased AWA.Workspaces.Module.Workspace_Module;
      Blog_Module       : aliased AWA.Blogs.Module.Blog_Module;
      Mail_Module       : aliased AWA.Mail.Module.Mail_Module;
      Comment_Module    : aliased AWA.Comments.Module.Comment_Module;
      Storage_Module    : aliased AWA.Storages.Modules.Storage_Module;

      Microblog_Module  : aliased Atlas.Microblog.Modules.Microblog_Module;
--      XXX_Module    : aliased Atlas.XXX.Module.XXX_Module;
   end record;

end Atlas.Applications;
