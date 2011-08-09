-----------------------------------------------------------------------
--  awa-blogs-module -- Blog and post management module
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

with AWA.Modules;
with ASF.Applications.Main;

with AWA.Blogs.Services;

with Util.Log.Loggers;

--  The <b>Blogs.Module</b> manages the creation, update, removal of blog posts in an application.
--
package body AWA.Blogs.Module is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Blogs.Module");

   --  Initialize the blog module.
   overriding
   procedure Initialize (Plugin : in out Blog_Module;
                         App    : access ASF.Applications.Main.Application'Class) is
   begin
      Log.Info ("Initializing the blogs module");

      --  Setup the resource bundles.
      App.Register ("blogMsg", "blogs");

--        Register.Register (Plugin  => Plugin,
--                           Name    => "AWA.Users.Beans.Authenticate_Bean",
--                           Handler => AWA.Users.Beans.Create_Authenticate_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App);

      --  Create the user manager when everything is initialized.
      Plugin.Manager := Plugin.Create_Blog_Manager;
   end Initialize;

   --  Get the blog manager.
   function Get_Blog_Manager (Plugin : in Blog_Module) return Services.Blog_Service_Access is
   begin
      return Plugin.Manager;
   end Get_Blog_Manager;

   --  Create a user manager.  This operation can be overriden to provide another
   --  user service implementation.
   function Create_Blog_Manager (Plugin : in Blog_Module) return Services.Blog_Service_Access is
      Result : constant Services.Blog_Service_Access := new Services.Blog_Service;
   begin
      Result.Initialize (Plugin);
      return Result;
   end Create_Blog_Manager;

   --  Get the user module instance associated with the current application.
   --     function Get_User_Module return User_Module_Access;

   --  Get the user manager instance associated with the current application.
   --     function Get_User_Manager return Services.User_Service_Access;

end AWA.Blogs.Module;