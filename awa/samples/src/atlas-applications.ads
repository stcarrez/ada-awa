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

with AWA.Applications;

with ASF.Servlets.Faces;
with ASF.Servlets.Files;
with ASF.Servlets.Ajax;
with ASF.Filters.Dump;
with ASF.Servlets.Measures;
with AWA.Users.Module;
with AWA.Comments.Module;
with AWA.Services.Filters;
package Atlas.Applications is

   type Application is new AWA.Applications.Application with private;
   type Application_Access is access all Application'Class;

   procedure Initialize (App : in Application_Access);

private

   type Application is new AWA.Applications.Application with record
      Faces          : aliased ASF.Servlets.Faces.Faces_Servlet;
      Files          : aliased ASF.Servlets.Files.File_Servlet;
      Ajax           : aliased ASF.Servlets.Ajax.Ajax_Servlet;
      Dump           : aliased ASF.Filters.Dump.Dump_Filter;
      Service_Filter : aliased AWA.Services.Filters.Service_Filter;
      User_Module    : aliased AWA.Users.Module.User_Module;
      Comment_Module : aliased AWA.Comments.Module.Comment_Module;
      Perf           : aliased ASF.Servlets.Measures.Measure_Servlet;
   end record;

end Atlas.Applications;
