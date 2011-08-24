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

with ADO.Drivers;

with ASF.Server.Tests;

with ASF.Tests;

with AWA.Users.Module;
with AWA.Mail.Module;
with AWA.Blogs.Module;

--  with AWA.Applications;
with AWA.Applications.Factory;
with AWA.Services.Filters;
package body AWA.Tests is

   App      : AWA.Applications.Application_Access := null;

   Factory        : AWA.Applications.Factory.Application_Factory;

   Service_Filter : aliased AWA.Services.Filters.Service_Filter;

   Users          : aliased AWA.Users.Module.User_Module;

   Mail           : aliased AWA.Mail.Module.Mail_Module;

   Blogs          : aliased AWA.Blogs.Module.Blog_Module;

   --  ------------------------------
   --  Initialize the awa test framework mockup.
   --  ------------------------------
   procedure Initialize (Props : in Util.Properties.Manager) is
      use AWA.Applications;
   begin
      ADO.Drivers.Initialize (Props);

      App := new AWA.Applications.Application;

      ASF.Tests.Initialize (Props, App.all'Access, Factory);
      App.Add_Filter ("service", Service_Filter'Access);
      App.Add_Filter_Mapping (Name => "service", Pattern => "*.html");

      declare
         Users : constant AWA.Users.Module.User_Module_Access := AWA.Tests.Users'Access;
      begin
         Register (App    => App.all'Access,
                   Name   => AWA.Users.Module.NAME,
                   URI    => "user",
                   Module => Users.all'Access);

         Register (App    => App.all'Access,
                   Name   => "mail",
                   URI    => "mail",
                   Module => Mail'Access);

         Register (App    => App.all'Access,
                   Name   => AWA.Blogs.Module.NAME,
                   URI    => "blogs",
                   Module => Blogs'Access);

         --           if Props.Exists ("test.server") then
         --              declare
         --                 WS : ASF.Server.Web.AWS_Container;
         --              begin
         --                 WS.Register_Application ("/awa", App.all'Access);
         --
         --                 WS.Start;
         --                 delay 600.0;
         --              end;
         --           end if;
         ASF.Server.Tests.Set_Context (App.all'Access);
      end;
   end Initialize;

   --  ------------------------------
   --  Get the test application.
   --  ------------------------------
   function Get_Application return AWA.Applications.Application_Access is
   begin
      return App;
   end Get_Application;

   --  ------------------------------
   --  Set the application context to simulate a web request context.
   --  ------------------------------
   procedure Set_Application_Context is
   begin
      ASF.Server.Tests.Set_Context (App.all'Access);
   end Set_Application_Context;

end AWA.Tests;
