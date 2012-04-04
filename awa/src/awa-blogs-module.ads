-----------------------------------------------------------------------
--  awa-blogs-module -- Blog and post management module
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with ASF.Applications;

with AWA.Modules;
with AWA.Blogs.Services;

--  The <b>Blogs.Module</b> manages the creation, update, removal of blog posts in an application.
--
package AWA.Blogs.Module is

   NAME : constant String := "blogs";

   type Blog_Module is new AWA.Modules.Module with private;
   type Blog_Module_Access is access all Blog_Module'Class;

   --  Initialize the blog module.
   overriding
   procedure Initialize (Plugin : in out Blog_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the blog manager.
   function Get_Blog_Manager (Plugin : in Blog_Module) return Services.Blog_Service_Access;

   --  Create a user manager.  This operation can be overriden to provide another
   --  user service implementation.
   function Create_Blog_Manager (Plugin : in Blog_Module) return Services.Blog_Service_Access;

   --  Get the blog module instance associated with the current application.
   function Get_Blog_Module return Blog_Module_Access;

   --  Get the blog manager instance associated with the current application.
   function Get_Blog_Manager return Services.Blog_Service_Access;

private

   type Blog_Module is new AWA.Modules.Module with record
      Manager     : Services.Blog_Service_Access := null;
   end record;

end AWA.Blogs.Module;