-----------------------------------------------------------------------
--  awa-blogs-module -- Blog and post management module
--  Copyright (C) 2011, 2012, 2013, 2014 Stephane Carrez
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

with ADO;
with AWA.Modules;
with AWA.Blogs.Models;

with Security.Permissions;

--  == Integration ==
--  The <tt>Blog_Module</tt> manages the creation, update, removal of blog posts in an application.
--  It provides operations that are used by the blog beans or other services to create and update
--  posts.  An instance of the <tt>Blog_Module</tt> must be declared and registered in the
--  AWA application.  The module instance can be defined as follows:
--
--    type Application is new AWA.Applications.Application with record
--       Blog_Module : aliased AWA.Blogs.Modules.Blog_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Blogs.Modules.NAME,
--              URI    => "blogs",
--              Module => App.Blog_Module'Access);
--
package AWA.Blogs.Modules is

   NAME : constant String := "blogs";

   --  Define the permissions.
   package ACL_Create_Blog is new Security.Permissions.Definition ("blog-create");
   package ACL_Delete_Blog is new Security.Permissions.Definition ("blog-delete");
   package ACL_Create_Post is new Security.Permissions.Definition ("blog-create-post");
   package ACL_Delete_Post is new Security.Permissions.Definition ("blog-delete-post");
   package ACL_Update_Post is new Security.Permissions.Definition ("blog-update-post");

   Not_Found : exception;

   type Blog_Module is new AWA.Modules.Module with private;
   type Blog_Module_Access is access all Blog_Module'Class;

   --  Initialize the blog module.
   overriding
   procedure Initialize (Plugin : in out Blog_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the blog module instance associated with the current application.
   function Get_Blog_Module return Blog_Module_Access;

   --  Create a new blog for the user workspace.
   procedure Create_Blog (Model        : in Blog_Module;
                          Workspace_Id : in ADO.Identifier;
                          Title        : in String;
                          Result       : out ADO.Identifier);

   --  Create a new post associated with the given blog identifier.
   procedure Create_Post (Model   : in Blog_Module;
                          Blog_Id : in ADO.Identifier;
                          Title   : in String;
                          URI     : in String;
                          Text    : in String;
                          Comment : in Boolean;
                          Status  : in AWA.Blogs.Models.Post_Status_Type;
                          Result  : out ADO.Identifier);

   --  Update the post title and text associated with the blog post identified by <b>Post</b>.
   procedure Update_Post (Model   : in Blog_Module;
                          Post_Id : in ADO.Identifier;
                          Title   : in String;
                          URI     : in String;
                          Text    : in String;
                          Comment : in Boolean;
                          Status  : in AWA.Blogs.Models.Post_Status_Type);

   --  Delete the post identified by the given identifier.
   procedure Delete_Post (Model   : in Blog_Module;
                          Post_Id : in ADO.Identifier);

private

   type Blog_Module is new AWA.Modules.Module with null record;

end AWA.Blogs.Modules;