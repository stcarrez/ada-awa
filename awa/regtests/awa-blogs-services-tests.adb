-----------------------------------------------------------------------
--  awa-blogs-tests -- Unit tests for blogs module
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

with Util.Tests;
with Util.Test_Caller;
--  with Util.Measures;

with ADO;

with Security.Contexts;

with AWA.Services.Contexts;
with AWA.Blogs.Module;
with AWA.Tests.Helpers.Users;
package body AWA.Blogs.Services.Tests is

   use Util.Tests;
   use ADO;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
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
      Manager   : AWA.Blogs.Services.Blog_Service_Access;
      Blog_Id   : ADO.Identifier;
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-blog@test.com");

      Manager := AWA.Blogs.Module.Get_Blog_Manager;
      Manager.Create_Blog (Workspace_Id => 0,
                           Title        => "My blog",
                           Result       => Blog_Id);
      T.Assert (Blog_Id > 0, "Invalid blog identifier");
   end Test_Create_Blog;

   --  ------------------------------
   --  Test creating and updating of a blog post
   --  ------------------------------
   procedure Test_Create_Post (T : in out Test) is
      Manager   : AWA.Blogs.Services.Blog_Service_Access;
      Blog_Id   : ADO.Identifier;
      Post_Id   : ADO.Identifier;
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-blog-post@test.com");

      Manager := AWA.Blogs.Module.Get_Blog_Manager;
      Manager.Create_Blog (Workspace_Id => 0,
                           Title        => "My blog post",
                           Result       => Blog_Id);
      T.Assert (Blog_Id > 0, "Invalid blog identifier");

      for I in 1 .. 5 loop
         Manager.Create_Post (Blog_Id => Blog_Id,
                              Title   => "Testing blog title",
                              URI     => "testing-blog-title",
                              Text    => "The blog content",
                              Result  => Post_Id);
         T.Assert (Post_Id > 0, "Invalid post identifier");

         Manager.Update_Post (Post_Id => Post_Id,
                              Title   => "New blog post title",
                              Text    => "The new post content");

         --  Keep the last post in the database.
         exit when I = 5;

         Manager.Delete_Post (Post_Id => Post_Id);

         --  Verify that a Not_Found exception is raised if the post was deleted.
         begin
            Manager.Update_Post (Post_Id => Post_Id,
                                 Title   => "Something",
                                 Text    => "Content");
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

end AWA.Blogs.Services.Tests;
