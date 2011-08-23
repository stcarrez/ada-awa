-----------------------------------------------------------------------
--  awa-blogs-services -- Blogs and post management
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

with AWA.Blogs.Models;
with AWA.Services.Contexts;
with AWA.Permissions;
with AWA.Permissions.Services;

with ADO.Sessions;

with Ada.Calendar;

with Util.Log.Loggers;

--  The <b>Blogs.Services</b> package defines the service and operations to
--  create, update and delete a post.
package body AWA.Blogs.Services is

   use AWA.Services;
   use ADO.Sessions;
   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Blogs.Services");

   --  Create a new blog for the user workspace.
   procedure Create_Blog (Model        : in Blog_Service;
                          Workspace_Id : in ADO.Identifier;
                          Title        : in String;
                          Result       : out ADO.Identifier) is
      use type ADO.Identifier;

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Blog  : AWA.Blogs.Models.Blog_Ref;
   begin
      Log.Info ("Creating blog for user");

      --  Check that the user has the create blog permission on the given workspace.
      AWA.Permissions.Check (ACL_Create_Blog.Permission, Workspace_Id);

      Ctx.Start;
      Blog.Set_Name (Title);
      Blog.Save (DB);

      AWA.Permissions.Services.Add_Permission (Session    => DB,
                                               User       => User,
                                               Entity     => Blog);
      Ctx.Commit;

      Result := Blog.Get_Id;
      Log.Info ("Blog {0} created for user {1}",
                ADO.Identifier'Image (Result), ADO.Identifier'Image (User));
   end Create_Blog;

   --  Create a new post associated with the given blog identifier.
   procedure Create_Post (Model   : in Blog_Service;
                          Blog_Id : in ADO.Identifier;
                          Title   : in String;
                          URI     : in String;
                          Text    : in String;
                          Result  : out ADO.Identifier) is
      use type ADO.Identifier;

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Blog  : AWA.Blogs.Models.Blog_Ref;
      Post  : AWA.Blogs.Models.Post_Ref;
      Found : Boolean;
   begin
      Log.Info ("Creating post for user");

      --  Check that the user has the create post permission on the given blog.
      AWA.Permissions.Check (ACL_Create_Post.Permission, Blog_Id);

      Ctx.Start;
      if Blog_Id > 0 then
         Blog.Load (Session => DB,
                    Id      => Blog_Id,
                    Found   => Found);
         if not Found then
            Log.Error ("Blog {0} not found", ADO.Identifier'Image (Blog_Id));
            raise Not_Found with "Blog not found";
         end if;
      else
         null;
      end if;

      Post.Set_Title (Title);
      Post.Set_Text (Text);
      Post.Set_Create_Date (Ada.Calendar.Clock);
      Post.Set_Uri (URI);
      Post.Set_Author (Ctx.Get_User);
      Post.Save (DB);
      Ctx.Commit;

      Result := Post.Get_Id;
      Log.Info ("Post {0} created for user {1}",
                ADO.Identifier'Image (Result), ADO.Identifier'Image (User));
   end Create_Post;

   --  Update the post title and text associated with the blog post identified by <b>Post</b>.
   procedure Update_Post (Model   : in Blog_Service;
                          Post_Id : in ADO.Identifier;
                          Title   : in String;
                          Text    : in String) is
      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Post  : AWA.Blogs.Models.Post_Ref;
      Found : Boolean;
   begin

      --  Check that the user has the update post permission on the given post.
      AWA.Permissions.Check (ACL_Update_Post.Permission, Post_Id);

      Ctx.Start;
      Post.Load (Session => DB, Id => Post_Id, Found => Found);
      if not Found then
         raise Not_Found;
      end if;
      Post.Set_Title (Title);
      Post.Set_Text (Text);
      Post.Save (DB);
      Ctx.Commit;
   end Update_Post;

   --  Delete the post identified by the given identifier.
   procedure Delete_Post (Model   : in Blog_Service;
                          Id      : in ADO.Identifier) is
   begin
      --  Check that the user has the delete post permission on the given post.
      AWA.Permissions.Check (ACL_Delete_Post.Permission, Id);

   end Delete_Post;

end AWA.Blogs.Services;
