-----------------------------------------------------------------------
--  awa-blogs-tests -- Unit tests for blogs module
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2019, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;

with ADO;

with Security.Contexts;

with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
package body AWA.Blogs.Modules.Tests is

   use ADO;

   package Caller is new Util.Test_Caller (Test, "Blogs.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Blogs.Services.Create_Blog",
                       Test_Create_Blog'Access);
      Caller.Add_Test (Suite, "Test AWA.Blogs.Services.Create_Post",
                       Test_Create_Post'Access);

   end Add_Tests;

   --  ------------------------------
   --  Test creation of a blog
   --  ------------------------------
   procedure Test_Create_Blog (T : in out Test) is
      Manager   : AWA.Blogs.Modules.Blog_Module_Access;
      Blog_Id   : ADO.Identifier;
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-blog@test.com");

      Manager := AWA.Blogs.Modules.Get_Blog_Module;
      Manager.Create_Blog (Title        => "My blog",
                           Result       => Blog_Id);
      T.Assert (Blog_Id > 0, "Invalid blog identifier");
   end Test_Create_Blog;

   --  ------------------------------
   --  Test creating and updating of a blog post
   --  ------------------------------
   procedure Test_Create_Post (T : in out Test) is
      Manager   : AWA.Blogs.Modules.Blog_Module_Access;
      Blog_Id   : ADO.Identifier;
      Post_Id   : ADO.Identifier;
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-blog-post@test.com");

      Manager := AWA.Blogs.Modules.Get_Blog_Module;
      Manager.Create_Blog (Title        => "My blog post",
                           Result       => Blog_Id);
      T.Assert (Blog_Id > 0, "Invalid blog identifier");

      for I in 1 .. 5 loop
         Manager.Create_Post (Blog_Id => Blog_Id,
                              Title   => "Testing blog title",
                              URI     => "testing-blog-title",
                              Text    => "The blog content",
                              Summary => "Summary",
                              Format  => AWA.Blogs.Models.FORMAT_DOTCLEAR,
                              Comment => False,
                              Status  => AWA.Blogs.Models.POST_DRAFT,
                              Result  => Post_Id);
         T.Assert (Post_Id > 0, "Invalid post identifier");

         Manager.Update_Post (Post_Id => Post_Id,
                              Title   => "New blog post title",
                              URI     => "testing-blog-title",
                              Text    => "The new post content",
                              Summary => "New summary",
                              Format  => AWA.Blogs.Models.FORMAT_DOTCLEAR,
                              Publish_Date => ADO.Nullable_Time '(Is_Null => True, others => <>),
                              Comment => True,
                              Status  => AWA.Blogs.Models.POST_DRAFT);

         --  Keep the last post in the database.
         exit when I = 5;

         Manager.Delete_Post (Post_Id => Post_Id);

         --  Verify that a Not_Found exception is raised if the post was deleted.
         begin
            Manager.Update_Post (Post_Id => Post_Id,
                                 Title   => "Something",
                                 Text    => "Content",
                                 Summary => "Summary",
                                 Format  => AWA.Blogs.Models.FORMAT_DOTCLEAR,
                                 URI     => "testing-blog-title",
                                 Publish_Date => ADO.Nullable_Time '(Is_Null => True,
                                                                     others => <>),
                                 Comment => True,
                                 Status  => AWA.Blogs.Models.POST_DRAFT);
            T.Assert (False, "Exception Not_Found was not raised");
         exception
            when Not_Found =>
               null;
         end;

         --  Verify that a Not_Found exception is raised if the post was deleted.
         begin
            Manager.Delete_Post (Post_Id => Post_Id);
            T.Assert (False, "Exception Not_Found was not raised");
         exception
            when Not_Found =>
               null;
         end;
      end loop;

   end Test_Create_Post;

end AWA.Blogs.Modules.Tests;
