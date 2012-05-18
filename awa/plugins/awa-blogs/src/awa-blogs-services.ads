-----------------------------------------------------------------------
--  awa-blogs-services -- Blogs and post management
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
with ADO;

with AWA.Modules;
with AWA.Blogs.Models;

with Security.Permissions;

--  The <b>Blogs.Services</b> package defines the service and operations to
--  create, update and delete a post.
package AWA.Blogs.Services is

   NAME : constant String := "Blog_Service";

   --  Define the permissions.
   package ACL_Create_Blog is new Security.Permissions.Permission_ACL ("blog-create");
   package ACL_Delete_Blog is new Security.Permissions.Permission_ACL ("blog-delete");
   package ACL_Create_Post is new Security.Permissions.Permission_ACL ("blog-create-post");
   package ACL_Delete_Post is new Security.Permissions.Permission_ACL ("blog-delete-post");
   package ACL_Update_Post is new Security.Permissions.Permission_ACL ("blog-update-post");

   --  Exception raised when a post cannot be found.
   Not_Found : exception;

   type Blog_Service is new AWA.Modules.Module_Manager with private;
   type Blog_Service_Access is access all Blog_Service'Class;

   --  Create a new blog for the user workspace.
   procedure Create_Blog (Model        : in Blog_Service;
                          Workspace_Id : in ADO.Identifier;
                          Title        : in String;
                          Result       : out ADO.Identifier);

   --  Create a new post associated with the given blog identifier.
   procedure Create_Post (Model   : in Blog_Service;
                          Blog_Id : in ADO.Identifier;
                          Title   : in String;
                          URI     : in String;
                          Text    : in String;
                          Status  : in AWA.Blogs.Models.Post_Status_Type;
                          Result  : out ADO.Identifier);

   --  Update the post title and text associated with the blog post identified by <b>Post</b>.
   procedure Update_Post (Model   : in Blog_Service;
                          Post_Id : in ADO.Identifier;
                          Title   : in String;
                          Text    : in String;
                          Status  : in AWA.Blogs.Models.Post_Status_Type);

   --  Delete the post identified by the given identifier.
   procedure Delete_Post (Model   : in Blog_Service;
                          Post_Id : in ADO.Identifier);

private

   type Blog_Service is new AWA.Modules.Module_Manager with null record;

end AWA.Blogs.Services;
