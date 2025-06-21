-----------------------------------------------------------------------
--  awa-blogs-tests -- Unit tests for blogs module
--  Copyright (C) 2017, 2018, 2019, 2020, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Strings;
with Util.Test_Caller;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with Security.Contexts;
with ASF.Tests;
with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with AWA.Storages.Modules;
with AWA.Storages.Beans;
with AWA.Storages.Models;
with AWA.Storages.Services;

package body AWA.Blogs.Tests is

   use Ada.Strings.Unbounded;
   use AWA.Tests;
   use type AWA.Storages.Services.Storage_Service_Access;

   package Caller is new Util.Test_Caller (Test, "Blogs.Beans");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Blogs.Beans.List_Post (Anonymous)",
                       Test_Anonymous_Access'Access);
      Caller.Add_Test (Suite, "Test AWA.Blogs.Beans.Create_Blog",
                       Test_Create_Blog'Access);
      Caller.Add_Test (Suite, "Test AWA.Blogs.Beans.Update_Post",
                       Test_Update_Post'Access);
      Caller.Add_Test (Suite, "Test AWA.Blogs.Beans.List_Post (Admin)",
                       Test_Admin_List_Posts'Access);
      Caller.Add_Test (Suite, "Test AWA.Blogs.Beans.List_Comments (Admin)",
                       Test_Admin_List_Comments'Access);
      Caller.Add_Test (Suite, "Test AWA.Blogs.Beans.Stats (Admin)",
                       Test_Admin_Blog_Stats'Access);
      Caller.Add_Test (Suite, "Test AWA.Blogs.Beans.Update_Post (Publish_Date)",
                       Test_Update_Publish_Date'Access);
      Caller.Add_Test (Suite, "Test AWA.Blogs.Servlets.Load",
                       Test_Image_Blog'Access);
   end Add_Tests;

   --  ------------------------------
   --  Get some access on the blog as anonymous users.
   --  ------------------------------
   procedure Verify_Anonymous (T    : in out Test;
                               Post : in String) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/blogs/view.html", "blog-view.html");
      ASF.Tests.Assert_Contains (T, "Blog posts", Reply, "Blog view page is invalid");

      ASF.Tests.Do_Get (Request, Reply, "/blogs/tagged.html?tag=test", "blog-tagged.html");
      ASF.Tests.Assert_Contains (T, "Tag - test", Reply, "Blog tag page is invalid");

      ASF.Tests.Do_Get (Request, Reply, "/blogs/post.html?post=missing", "blog-missing.html");
      ASF.Tests.Assert_Matches (T, "The post you are looking for does not exist",
                                Reply, "Blog post missing page is invalid",
                                ASF.Responses.SC_NOT_FOUND);

      if Post = "" then
         return;
      end if;

      ASF.Tests.Do_Get (Request, Reply, "/blogs/post.html?post=" & Post, "blog-post.html");
      ASF.Tests.Assert_Matches (T, ".*The blog post.*content.*", Reply,
                                "Blog post page is invalid"
                                );

   end Verify_Anonymous;

   --  ------------------------------
   --  Test access to the blog as anonymous user.
   --  ------------------------------
   procedure Test_Anonymous_Access (T : in out Test) is
   begin
      T.Verify_Anonymous ("");
   end Test_Anonymous_Access;

   --  ------------------------------
   --  Test creation of blog by simulating web requests.
   --  ------------------------------
   procedure Test_Create_Blog (T : in out Test) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
      Uuid      : constant String := Util.Tests.Get_Uuid;
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/blogs/admin/create-blog.html",
                        "create-blog-get.html");
      T.Assert (Reply.Get_Status = ASF.Responses.SC_OK,
                "Invalid response after getting blog creation page");
      ASF.Tests.Assert_Matches (T, "<dl id=.title.*<dt><label for=.title.*"
                                & "<dd><input type=.text.*", Reply,
                                "Blog post admin page is missing input field");

      Request.Set_Parameter ("title", "The Blog Title");
      Request.Set_Parameter ("create", "1");
      ASF.Tests.Set_CSRF (Request, "create-blog", "create-blog-get.html");
      ASF.Tests.Do_Post (Request, Reply, "/blogs/admin/create-blog.html", "create-blog.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY,
                "Invalid response after blog creation");
      declare
         Ident : constant String
            := Helpers.Extract_Redirect (Reply, "/asfunit/blogs/admin/create.html?id=");
         Post_Ident : Unbounded_String;
      begin
         Util.Tests.Assert_Matches (T, "^[0-9]+$", Ident,
                                    "Invalid blog identifier in the response");
         T.Blog_Ident := To_Unbounded_String (Ident);

         ASF.Tests.Do_Get (Request, Reply, "/blogs/admin/create.html?id=" & Ident,
                           "create-post-get.html");

         Request.Set_Parameter ("post-blog-id", Ident);
         Request.Set_Parameter ("post-title", "Post title");
         Request.Set_Parameter ("text", "The blog post content.");
         Request.Set_Parameter ("uri", Uuid);
         Request.Set_Parameter ("post-create", "1");
         Request.Set_Parameter ("post-status", "POST_PUBLISHED");
         Request.Set_Parameter ("allow-comment", "0");
         Request.Set_Parameter ("post-format", "FORMAT_DOTCLEAR");
         ASF.Tests.Set_CSRF (Request, "post", "create-post-get.html");
         ASF.Tests.Do_Post (Request, Reply, "/blogs/admin/create.html", "create-post.html");

         T.Post_Ident := Helpers.Extract_Redirect (Reply, "/asfunit/blogs/admin/edit/"
                                                   & Ident & "/");

         Util.Tests.Assert_Matches (T, "^[0-9]+$", To_String (T.Post_Ident),
                                    "Invalid post identifier in the response");

         Request.Set_Parameter ("post-blog-id", Ident);
         Request.Set_Parameter ("post-id", To_String (T.Post_Ident));
         Request.Set_Parameter ("post-title", "Post title");
         Request.Set_Parameter ("text", "The blog post content.");
         Request.Set_Parameter ("uri", Uuid);
         Request.Set_Parameter ("save", "1");
         Request.Set_Parameter ("post-status", "POST_PUBLISHED");
         Request.Set_Parameter ("post-format", "FORMAT_DOTCLEAR");
         Request.Set_Parameter ("allow-comment", "0");
         ASF.Tests.Set_CSRF (Request, "post", "create-post-get.html");
         ASF.Tests.Do_Post (Request, Reply, "/blogs/admin/edit.html", "create-edit-post.html");

         Post_Ident := Helpers.Extract_Redirect (Reply, "/asfunit/blogs/admin/"
                                                 & Ident & "/preview/");
         Util.Tests.Assert_Equals (T, To_String (T.Post_Ident), Post_Ident,
                                   "Invalid post ident after edit");

      end;

      --  Check public access to the post.
      T.Post_Uri := To_Unbounded_String (Uuid);
      T.Verify_Anonymous (Uuid);
   end Test_Create_Blog;

   --  ------------------------------
   --  Test updating a post by simulating web requests.
   --  ------------------------------
   procedure Test_Update_Post (T : in out Test) is
      Request    : ASF.Requests.Mockup.Request;
      Reply      : ASF.Responses.Mockup.Response;
      Uuid       : constant String := Util.Tests.Get_Uuid;
      Ident      : constant String := To_String (T.Blog_Ident);
      Post_Ident : Unbounded_String;
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/blogs/admin/edit/" & Ident & "/"
                        & To_String (T.Post_Ident), "edit-post-get.html");

      Request.Set_Parameter ("post-blog-id", Ident);
      Request.Set_Parameter ("post-id", To_String (T.Post_Ident));
      Request.Set_Parameter ("post-title", "New post title");
      Request.Set_Parameter ("text", "The blog post new content.");
      Request.Set_Parameter ("uri", Uuid);
      Request.Set_Parameter ("save", "1");
      Request.Set_Parameter ("post-status", "POST_PUBLISHED");
      Request.Set_Parameter ("post-format", "FORMAT_DOTCLEAR");
      Request.Set_Parameter ("allow-comment", "0");
      ASF.Tests.Set_CSRF (Request, "post", "edit-post-get.html");
      ASF.Tests.Do_Post (Request, Reply, "/blogs/admin/edit.html", "edit-post.html");

      Post_Ident := Helpers.Extract_Redirect (Reply, "/asfunit/blogs/admin/"
                                              & Ident & "/preview/");

      Util.Tests.Assert_Equals (T, To_String (T.Post_Ident), To_String (Post_Ident),
                                "Invalid post identifier returned after post update");
      T.Verify_Anonymous (Uuid);
   end Test_Update_Post;

   --  ------------------------------
   --  Test updating the publication date by simulating web requests.
   --  ------------------------------
   procedure Test_Update_Publish_Date (T : in out Test) is
      Request    : ASF.Requests.Mockup.Request;
      Reply      : ASF.Responses.Mockup.Response;
      Uuid       : constant String := Util.Tests.Get_Uuid;
      Ident      : constant String := To_String (T.Blog_Ident);
      Post_Ident : Unbounded_String;
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      Request.Set_Parameter ("post_id", To_String (T.Post_Ident));
      Request.Set_Parameter ("blog_id", Ident);
      ASF.Tests.Do_Get (Request, Reply, "/blogs/admin/edit.html", "edit-post-form-get.html");

      Request.Set_Parameter ("post-blog-id", Ident);
      Request.Set_Parameter ("post-id", To_String (T.Post_Ident));
      Request.Set_Parameter ("post-title", "New post title");
      Request.Set_Parameter ("text", "The blog post new content.");
      Request.Set_Parameter ("uri", Uuid);
      Request.Set_Parameter ("save", "1");
      Request.Set_Parameter ("post-format", "FORMAT_DOTCLEAR");
      Request.Set_Parameter ("post-status", "POST_PUBLISHED");
      Request.Set_Parameter ("allow-comment", "0");
      Request.Set_Parameter ("publish-date", "");
      ASF.Tests.Set_CSRF (Request, "post", "edit-post-form-get.html");
      ASF.Tests.Do_Post (Request, Reply, "/blogs/admin/edit.html", "edit-post.html");

      Post_Ident := Helpers.Extract_Redirect (Reply, "/asfunit/blogs/admin/"
                                              & Ident & "/preview/");

      Util.Tests.Assert_Equals (T, To_String (T.Post_Ident), To_String (Post_Ident),
                                "Invalid post identifier returned after post update");
      T.Verify_Anonymous (Uuid);
   end Test_Update_Publish_Date;

   --  ------------------------------
   --  Test listing the blog posts.
   --  ------------------------------
   procedure Test_Admin_List_Posts (T : in out Test) is
      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Ident   : constant String := To_String (T.Blog_Ident);
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/blogs/admin/list.html?id=" & Ident, "blog-list.html");
      ASF.Tests.Assert_Contains (T, "blog-post-list-header", Reply, "Blog admin page is invalid");
   end Test_Admin_List_Posts;

   --  ------------------------------
   --  Test listing the blog comments.
   --  ------------------------------
   procedure Test_Admin_List_Comments (T : in out Test) is
      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Ident   : constant String := To_String (T.Blog_Ident);
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/blogs/admin/list-comments.html?id=" & Ident,
                        "blog-list-comments.html");
      ASF.Tests.Assert_Contains (T, "blog-comment-list-header", Reply,
                                 "Blog admin comments page is invalid");
   end Test_Admin_List_Comments;

   --  ------------------------------
   --  Test getting the JSON blog stats (for graphs).
   --  ------------------------------
   procedure Test_Admin_Blog_Stats (T : in out Test) is
      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Ident   : constant String := To_String (T.Blog_Ident);
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/blogs/admin/" & Ident & "/stats",
                        "blog-stats.html");
      ASF.Tests.Assert_Contains (T, "data", Reply,
                                 "Blog admin stats page is invalid");
   end Test_Admin_Blog_Stats;

   --  ------------------------------
   --  Test getting an image from the blog servlet.
   --  ------------------------------
   procedure Test_Image_Blog (T : in out Test) is
      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Post_Id : constant String := To_String (T.Post_Ident);
      Sec_Ctx : Security.Contexts.Security_Context;
      Context : AWA.Services.Contexts.Service_Context;
      Folder  : AWA.Storages.Beans.Folder_Bean;
      Store   : AWA.Storages.Models.Storage_Ref;
      Mgr     : AWA.Storages.Services.Storage_Service_Access;
      Outcome : Ada.Strings.Unbounded.Unbounded_String;
      Path    : constant String
        := Util.Tests.Get_Path ("regtests/files/images/Ada-Lovelace.jpg");
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

      Mgr := AWA.Storages.Modules.Get_Storage_Manager;
      T.Assert (Mgr /= null, "Null storage manager");

      --  Make a storage folder.
      Folder.Module := AWA.Storages.Modules.Get_Storage_Module;
      Folder.Set_Name ("Images");
      Folder.Save (Outcome);

      Store.Set_Folder (Folder);
      Store.Set_Mime_Type ("image/jpg");
      Store.Set_Name ("Ada-Lovelace.jpg");
      Mgr.Save (Store, Path, AWA.Storages.Models.FILE);

      --  First test to make sure we get a 404 error.
      ASF.Tests.Do_Get (Request, Reply, "/blogs/images/" & Post_Id & "/0/default/Ada-Lovelace.jpg",
                        "Ada-Lovelace.jpg");
      T.Assert (Reply.Get_Status = Servlet.Responses.SC_NOT_FOUND,
                "Invalid response after image get");

      --  Second test to get the image.
      declare
         Image_Ident : constant String := Util.Strings.Image (Natural (Store.Get_Id));
      begin
         ASF.Tests.Do_Get (Request, Reply, "/blogs/images/" & Post_Id & "/"
                           & Image_Ident & "/default/Ada-Lovelace.jpg",
                           "Ada-Lovelace.jpg");
         T.Assert (Reply.Get_Status = Servlet.Responses.SC_OK,
                   "Invalid response after image get");
      end;
   end Test_Image_Blog;

end AWA.Blogs.Tests;
