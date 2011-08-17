-----------------------------------------------------------------------
--  atlas-applications - Atlas Applications
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
with ASF.Applications;
with ASF.Locales;
with ASF.Applications.Main;

with AWA.Users.Module;
package body Atlas.Applications is

   use AWA.Applications;

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
      ADO.Drivers.Initialize;

      C.Load_Properties ("atlas.properties");
      App.Initialize (C, Fact);

      App.Register ("layoutMsg", "layout");
      App.Set_Global ("contextPath", "/atlas");
      App.Set_Global ("homeUrl", "/am/home/myhome");

      App.Set_Global ("version", "0.1");

      --  Register the servlets and filters
      App.Add_Servlet (Name => "faces", Server => App.Faces'Access);
      App.Add_Servlet (Name => "files", Server => App.Files'Access);
      App.Add_Servlet (Name => "perf", Server => App.Perf'Access);
      App.Add_Servlet (Name => "action", Server => App.Ajax'Access);
      App.Add_Filter (Name => "dump", Filter => App.Dump'Access);
      App.Add_Filter (Name => "service", Filter => App.Service_Filter'Access);
      App.Add_Filter (Name => "perf", Filter => App.Perf'Access);
      App.Add_Filter (Name => "service", Filter => App.Service_Filter'Access);

      --  Define servlet mappings
      App.Add_Mapping (Name => "action", Pattern => "/action/*");
      App.Add_Mapping (Name => "faces", Pattern => "*.html");
      App.Add_Mapping (Name => "files", Pattern => "*.css");
      App.Add_Mapping (Name => "files", Pattern => "*.js");
      App.Add_Mapping (Name => "files", Pattern => "*.png");
      App.Add_Mapping (Name => "files", Pattern => "*.jpg");
      App.Add_Mapping (Name => "perf", Pattern => "statistics.xml");

      App.Add_Filter_Mapping (Name => "perf", Pattern => "*.html");
      App.Add_Filter_Mapping (Name => "perf", Pattern => "*.css");
      App.Add_Filter_Mapping (Name => "perf", Pattern => "*.js");
      App.Add_Filter_Mapping (Name => "perf", Pattern => "*.png");
      App.Add_Filter_Mapping (Name => "perf", Pattern => "*.jpg");

      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");
      App.Add_Filter_Mapping (Name => "service", Pattern => "*.html");
      App.Add_Filter_Mapping (Name => "service", Pattern => "/action/*");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.css");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.png");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.jpg");

      Register (App    => App.all'Access,
                Name   => "users", URI => "users",
                Module => App.User_Module'Access);
      Register (App    => App.all'Access,
                Name   => "mail", URI => "mail",
                Module => App.Mail_Module'Access);
      Register (App    => App.all'Access,
                Name   => "blogs", URI => "blogs",
                Module => App.Blog_Module'Access);

      App.Add_Filter_Mapping (Name => "dump", Pattern => "/auth/verify");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "/auth/auth/*");
      --
      --     App.Add_Converter (Name => "float", Converter => Conv.all'Unchecked_Access);
   end Initialize;

end Atlas.Applications;
