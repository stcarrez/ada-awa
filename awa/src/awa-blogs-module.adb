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

with Util.Log.Loggers;

with AWA.Modules.Get;
with AWA.Modules.Beans;
with AWA.Blogs.Beans;
with AWA.Applications;

package body AWA.Blogs.Module is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Blogs.Module");

   package Register is new AWA.Modules.Beans (Module => Blog_Module,
                                              Module_Access => Blog_Module_Access);

   --  ------------------------------
   --  Initialize the blog module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Blog_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the blogs module");

      --  Setup the resource bundles.
      App.Register ("blogMsg", "blogs");

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Blogs.Beans.Post_Bean",
                         Handler => AWA.Blogs.Beans.Create_Post_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Blogs.Beans.Post_List_Bean",
                         Handler => AWA.Blogs.Beans.Create_Post_List_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Blogs.Beans.Admin_Post_List_Bean",
                         Handler => AWA.Blogs.Beans.Create_Admin_Post_List_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Blogs.Beans.Admin_Blog_List_Bean",
                         Handler => AWA.Blogs.Beans.Create_Blog_List_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Blogs.Beans.Blog_Bean",
                         Handler => AWA.Blogs.Beans.Create_Blog_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Create the user manager when everything is initialized.
      Plugin.Manager := Plugin.Create_Blog_Manager;
   end Initialize;

   --  ------------------------------
   --  Get the blog manager.
   --  ------------------------------
   function Get_Blog_Manager (Plugin : in Blog_Module) return Services.Blog_Service_Access is
   begin
      return Plugin.Manager;
   end Get_Blog_Manager;

   --  ------------------------------
   --  Create a user manager.  This operation can be overriden to provide another
   --  user service implementation.
   --  ------------------------------
   function Create_Blog_Manager (Plugin : in Blog_Module) return Services.Blog_Service_Access is
      Result : constant Services.Blog_Service_Access := new Services.Blog_Service;
   begin
      Result.Initialize (Plugin);
      return Result;
   end Create_Blog_Manager;

   --  ------------------------------
   --  Get the blog module instance associated with the current application.
   --  ------------------------------
   function Get_Blog_Module return Blog_Module_Access is
      function Get is new AWA.Modules.Get (Blog_Module, Blog_Module_Access, NAME);
   begin
      return Get;
   end Get_Blog_Module;

   --  ------------------------------
   --  Get the user manager instance associated with the current application.
   --  ------------------------------
   function Get_Blog_Manager return Services.Blog_Service_Access is
      Module : constant Blog_Module_Access := Get_Blog_Module;
   begin
      if Module = null then
         Log.Error ("There is no active Blog_Module");
         return null;
      else
         return Module.Get_Blog_Manager;
      end if;
   end Get_Blog_Manager;

end AWA.Blogs.Module;