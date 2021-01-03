-----------------------------------------------------------------------
--  awa-blogs-module -- Blog and post management module
--  Copyright (C) 2011, 2012, 2013, 2017, 2018, 2019, 2020 Stephane Carrez
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
with Util.Beans.Objects;
with Util.Beans.Basic;

with AWA.Modules.Get;
with AWA.Modules.Beans;
with AWA.Blogs.Beans;
with AWA.Applications;
with AWA.Services.Contexts;
with AWA.Permissions;
with AWA.Workspaces.Models;
with AWA.Workspaces.Modules;
with AWA.Storages.Models;

with ADO.Objects;
with ADO.Sessions;
with ADO.Statements;

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

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Blogs.Beans.Stat_Bean",
                         Handler => AWA.Blogs.Beans.Create_Blog_Stat_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Blogs.Beans.Format_List_Bean",
                         Handler => AWA.Blogs.Beans.Create_Format_List_Bean'Access);

      App.Add_Servlet ("blog-image", Plugin.Image_Servlet'Unchecked_Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Blog_Module;
                        Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);

      Image_Prefix : constant String := Plugin.Get_Config (PARAM_IMAGE_PREFIX);
   begin
      Plugin.Image_Prefix := Wiki.Strings.To_UString (Wiki.Strings.To_WString (Image_Prefix));
   end Configure;

   --  ------------------------------
   --  Get the blog module instance associated with the current application.
   --  ------------------------------
   function Get_Blog_Module return Blog_Module_Access is
      function Get is new AWA.Modules.Get (Blog_Module, Blog_Module_Access, NAME);
   begin
      return Get;
   end Get_Blog_Module;

   --  ------------------------------
   --  Get the image prefix that was configured for the Blog module.
   --  ------------------------------
   function Get_Image_Prefix (Module : in Blog_Module)
                              return Wiki.Strings.UString is
   begin
      return Module.Image_Prefix;
   end Get_Image_Prefix;

   procedure Publish_Post (Module : in Blog_Module;
                           Post   : in AWA.Blogs.Models.Post_Ref) is
      Current_Entity : aliased AWA.Blogs.Models.Post_Ref := Post;
      Ptr   : constant Util.Beans.Basic.Readonly_Bean_Access
        := Current_Entity'Unchecked_Access;
      Bean  : constant Util.Beans.Objects.Object
        := Util.Beans.Objects.To_Object (Ptr, Util.Beans.Objects.STATIC);
      Event : AWA.Events.Module_Event;
   begin
      Event.Set_Event_Kind (Post_Publish_Event.Kind);
      Event.Set_Parameter ("summary", Post.Get_Summary);
      Event.Set_Parameter ("title", Post.Get_Title);
      Event.Set_Parameter ("uri", Post.Get_Uri);
      Event.Set_Parameter ("post", Bean);
      Event.Set_Parameter ("author", Post.Get_Author.Get_Name);
      Module.Send_Event (Event);
   end Publish_Post;

   --  ------------------------------
   --  Create a new blog for the user workspace.
   --  ------------------------------
   procedure Create_Blog (Model        : in Blog_Module;
                          Title        : in String;
                          Result       : out ADO.Identifier) is
      pragma Unreferenced (Model);

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
      AWA.Workspaces.Modules.Add_Permission (Session   => DB,
                                             User      => User,
                                             Entity    => Blog,
                                             Workspace => WS.Get_Id,
                                             List      => (ACL_Delete_Blog.Permission,
                                                           ACL_Update_Post.Permission,
                                                           ACL_Create_Post.Permission,
                                                           ACL_Delete_Post.Permission));
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
                          Summary : in String;
                          Format  : in AWA.Blogs.Models.Format_Type;
                          Comment : in Boolean;
                          Status  : in AWA.Blogs.Models.Post_Status_Type;
                          Result  : out ADO.Identifier) is
      use type AWA.Blogs.Models.Post_Status_Type;

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
      Post.Set_Summary (Summary);
      Post.Set_Create_Date (Ada.Calendar.Clock);
      if Status = AWA.Blogs.Models.POST_PUBLISHED then
         Post.Set_Publish_Date (ADO.Nullable_Time '(Is_Null => False,
                                                    Value   => Ada.Calendar.Clock));
      end if;
      Post.Set_Uri (URI);
      Post.Set_Author (Ctx.Get_User);
      Post.Set_Status (Status);
      Post.Set_Blog (Blog);
      Post.Set_Format (Format);
      Post.Set_Allow_Comments (Comment);
      Post.Save (DB);
      Ctx.Commit;

      Result := Post.Get_Id;
      Log.Info ("Post {0} created for user {1}",
                ADO.Identifier'Image (Result), ADO.Identifier'Image (User));

      --  The post is published, post an event to trigger specific actions.
      if Status = AWA.Blogs.Models.POST_PUBLISHED then
         Publish_Post (Model, Post);
      end if;
   end Create_Post;

   --  ------------------------------
   --  Update the post title and text associated with the blog post identified by <b>Post</b>.
   --  ------------------------------
   procedure Update_Post (Model   : in Blog_Module;
                          Post_Id : in ADO.Identifier;
                          Title   : in String;
                          Summary : in String;
                          URI     : in String;
                          Text    : in String;
                          Format  : in AWA.Blogs.Models.Format_Type;
                          Comment : in Boolean;
                          Publish_Date : in ADO.Nullable_Time;
                          Status  : in AWA.Blogs.Models.Post_Status_Type) is
      pragma Unreferenced (Model);
      use type AWA.Blogs.Models.Post_Status_Type;

      Ctx   : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Post  : AWA.Blogs.Models.Post_Ref;
      Found : Boolean;
      Published : Boolean;
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

      if not Publish_Date.Is_Null then
         Post.Set_Publish_Date (Publish_Date);
      end if;
      Published := Status = AWA.Blogs.Models.POST_PUBLISHED and then Post.Get_Publish_Date.Is_Null;
      if Published then
         Post.Set_Publish_Date (ADO.Nullable_Time '(Is_Null => False,
                                                    Value   => Ada.Calendar.Clock));
      end if;
      Post.Set_Title (Title);
      Post.Set_Text (Text);
      Post.Set_Summary (Summary);
      Post.Set_Uri (URI);
      Post.Set_Status (Status);
      Post.Set_Format (Format);
      Post.Set_Allow_Comments (Comment);
      Post.Save (DB);
      Ctx.Commit;

      --  The post is published, post an event to trigger specific actions.
      if Published then
         Publish_Post (Model, Post);
      end if;
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

   --  ------------------------------
   --  Load the image data associated with a blog post.  The image must be public and the
   --  post visible for the image to be retrieved by anonymous users.
   --  ------------------------------
   procedure Load_Image (Model    : in Blog_Module;
                         Post_Id  : in ADO.Identifier;
                         Image_Id : in ADO.Identifier;
                         Width    : in out Natural;
                         Height   : in out Natural;
                         Mime     : out Ada.Strings.Unbounded.Unbounded_String;
                         Date     : out Ada.Calendar.Time;
                         Into     : out ADO.Blob_Ref) is
      pragma Unreferenced (Model);
      use type AWA.Storages.Models.Storage_Type;

      Ctx   : constant Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : constant ADO.Sessions.Session := AWA.Services.Contexts.Get_Session (Ctx);
      Query : ADO.Statements.Query_Statement;
      Kind  : AWA.Storages.Models.Storage_Type;
   begin
      if Width = Natural'Last or Height = Natural'Last then
         Query := DB.Create_Statement (Models.Query_Blog_Image_Get_Data);
      elsif Width > 0 then
         Query := DB.Create_Statement (Models.Query_Blog_Image_Width_Get_Data);
         Query.Bind_Param ("width", Width);
      elsif Height > 0 then
         Query := DB.Create_Statement (Models.Query_Blog_Image_Height_Get_Data);
         Query.Bind_Param ("height", Height);
      else
         Query := DB.Create_Statement (Models.Query_Blog_Image_Get_Data);
      end if;
      Query.Bind_Param ("post_id", Post_Id);
      Query.Bind_Param ("store_id", Image_Id);
      Query.Bind_Param ("user_id", User);
      Query.Execute;
      if not Query.Has_Elements then
         Log.Warn ("Blog post image entity {0} not found", ADO.Identifier'Image (Image_Id));
         raise ADO.Objects.NOT_FOUND;
      end if;
      Mime   := Query.Get_Unbounded_String (0);
      Date   := Query.Get_Time (1);
      Width  := Query.Get_Natural (4);
      Height := Query.Get_Natural (5);
      Kind   := AWA.Storages.Models.Storage_Type'Val (Query.Get_Integer (3));
      if Kind = AWA.Storages.Models.DATABASE then
         Into := Query.Get_Blob (6);
      else
         null;
      end if;
   end Load_Image;

end AWA.Blogs.Modules;
