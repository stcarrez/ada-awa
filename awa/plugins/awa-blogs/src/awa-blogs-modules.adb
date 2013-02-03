-----------------------------------------------------------------------
--  awa-blogs-module -- Blog and post management module
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
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
with AWA.Services.Contexts;
with AWA.Permissions;
with AWA.Permissions.Services;
with AWA.Workspaces.Models;
with AWA.Workspaces.Modules;

with ADO.Sessions;

with Ada.Calendar;

package body AWA.Blogs.Modules is

   package ASC renames AWA.Services.Contexts;

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
                         Name    => "AWA.Blogs.Beans.Blog_Admin_Bean",
                         Handler => AWA.Blogs.Beans.Create_Blog_Admin_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Blogs.Beans.Blog_Bean",
                         Handler => AWA.Blogs.Beans.Create_Blog_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Blogs.Beans.Status_List_Bean",
                         Handler => AWA.Blogs.Beans.Create_Status_List'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Get the blog module instance associated with the current application.
   --  ------------------------------
   function Get_Blog_Module return Blog_Module_Access is
      function Get is new AWA.Modules.Get (Blog_Module, Blog_Module_Access, NAME);
   begin
      return Get;
   end Get_Blog_Module;

   --  ------------------------------
   --  Create a new blog for the user workspace.
   --  ------------------------------
   procedure Create_Blog (Model        : in Blog_Module;
                          Workspace_Id : in ADO.Identifier;
                          Title        : in String;
                          Result       : out ADO.Identifier) is
      pragma Unreferenced (Model);
      use type ADO.Identifier;

      Ctx   : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Blog  : AWA.Blogs.Models.Blog_Ref;
      WS    : AWA.Workspaces.Models.Workspace_Ref;
   begin
      Log.Info ("Creating blog {0} for user", Title);

      Ctx.Start;
      AWA.Workspaces.Modules.Get_Workspace (DB, Ctx, WS);

      --  Check that the user has the create blog permission on the given workspace.
      AWA.Permissions.Check (Permission => ACL_Create_Blog.Permission,
                             Entity     => WS);

      Blog.Set_Name (Title);
      Blog.Set_Workspace (WS);
      Blog.Set_Create_Date (Ada.Calendar.Clock);
      Blog.Save (DB);

      --  Add the permission for the user to use the new blog.
      AWA.Permissions.Services.Add_Permission (Session => DB,
                                               User    => User,
                                               Entity  => Blog);
      Ctx.Commit;

      Result := Blog.Get_Id;
      Log.Info ("Blog {0} created for user {1}",
                ADO.Identifier'Image (Result), ADO.Identifier'Image (User));
   end Create_Blog;

   --  ------------------------------
   --  Create a new post associated with the given blog identifier.
   --  ------------------------------
   procedure Create_Post (Model   : in Blog_Module;
                          Blog_Id : in ADO.Identifier;
                          Title   : in String;
                          URI     : in String;
                          Text    : in String;
                          Status  : in AWA.Blogs.Models.Post_Status_Type;
                          Result  : out ADO.Identifier) is
      pragma Unreferenced (Model);
      use type ADO.Identifier;

      Ctx   : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Blog  : AWA.Blogs.Models.Blog_Ref;
      Post  : AWA.Blogs.Models.Post_Ref;
      Found : Boolean;
   begin
      Log.Debug ("Creating post for user");

      Ctx.Start;
      --  Get the blog instance.
      Blog.Load (Session => DB,
                 Id      => Blog_Id,
                 Found   => Found);
      if not Found then
         Log.Error ("Blog {0} not found", ADO.Identifier'Image (Blog_Id));
         raise Not_Found with "Blog not found";
      end if;

      --  Check that the user has the create post permission on the given blog.
      AWA.Permissions.Check (Permission => ACL_Create_Post.Permission,
                             Entity     => Blog);

      --  Build the new post.
      Post.Set_Title (Title);
      Post.Set_Text (Text);
      Post.Set_Create_Date (Ada.Calendar.Clock);
      Post.Set_Uri (URI);
      Post.Set_Author (Ctx.Get_User);
      Post.Set_Status (Status);
      Post.Set_Blog (Blog);
      Post.Save (DB);
      Ctx.Commit;

      Result := Post.Get_Id;
      Log.Info ("Post {0} created for user {1}",
                ADO.Identifier'Image (Result), ADO.Identifier'Image (User));
   end Create_Post;

   --  ------------------------------
   --  Update the post title and text associated with the blog post identified by <b>Post</b>.
   --  ------------------------------
   procedure Update_Post (Model   : in Blog_Module;
                          Post_Id : in ADO.Identifier;
                          Title   : in String;
                          Text    : in String;
                          Status  : in AWA.Blogs.Models.Post_Status_Type) is
      pragma Unreferenced (Model);

      Ctx   : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Post  : AWA.Blogs.Models.Post_Ref;
      Found : Boolean;
   begin
      Ctx.Start;
      Post.Load (Session => DB, Id => Post_Id, Found => Found);
      if not Found then
         Log.Error ("Post {0} not found", ADO.Identifier'Image (Post_Id));
         raise Not_Found;
      end if;

      --  Check that the user has the update post permission on the given post.
      AWA.Permissions.Check (Permission => ACL_Update_Post.Permission,
                             Entity     => Post);

      Post.Set_Title (Title);
      Post.Set_Text (Text);
      Post.Set_Status (Status);
      Post.Save (DB);
      Ctx.Commit;
   end Update_Post;

   --  ------------------------------
   --  Delete the post identified by the given identifier.
   --  ------------------------------
   procedure Delete_Post (Model   : in Blog_Module;
                          Post_Id : in ADO.Identifier) is
      pragma Unreferenced (Model);

      Ctx   : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Post  : AWA.Blogs.Models.Post_Ref;
      Found : Boolean;
   begin
      Ctx.Start;
      Post.Load (Session => DB, Id => Post_Id, Found => Found);
      if not Found then
         Log.Error ("Post {0} not found", ADO.Identifier'Image (Post_Id));
         raise Not_Found;
      end if;

      --  Check that the user has the delete post permission on the given post.
      AWA.Permissions.Check (Permission => ACL_Delete_Post.Permission,
                             Entity     => Post);

      Post.Delete (Session => DB);
      Ctx.Commit;
   end Delete_Post;

end AWA.Blogs.Modules;