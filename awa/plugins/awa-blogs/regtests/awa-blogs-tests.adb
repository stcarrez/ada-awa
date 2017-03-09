-----------------------------------------------------------------------
--  awa-blogs-tests -- Unit tests for blogs module
--  Copyright (C) 2017 Stephane Carrez
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

with Util.Test_Caller;
with Util.Strings;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Principals;
with ASF.Tests;
with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with Security.Contexts;

package body AWA.Blogs.Tests is

   package Caller is new Util.Test_Caller (Test, "Blogs.Beans");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Blogs.Beans.Create_Blog",
                       Test_Create_Blog'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of blog by simulating web requests.
   --  ------------------------------
   procedure Test_Create_Blog (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      AWA.Tests.Helpers.Users.Login ("test-wiki@test.com", Request);
      Request.Set_Parameter ("title", "The Blog Title");
      Request.Set_Parameter ("create-blog", "1");
      Request.Set_Parameter ("create", "1");
      ASF.Tests.Do_Post (Request, Reply, "/blogs/admin/create-blog.html", "create-blog.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY, "Invalid response");
      declare
         R   : constant String := Reply.Get_Header ("Location");
         Pos : constant Natural := Util.Strings.Rindex (R, '=');
      begin
         T.Assert (Pos > 0, "Invalid redirect response");
         ASF.Tests.Assert_Redirect (T, "/asfunit/blogs/admin/create.html?id="
                                    & R (Pos + 1 .. R'Last),
                                    Reply, "Invalid redirect after blog creation");

         Request.Set_Parameter ("post-blog-id", R (Pos + 1 .. R'Last));
         Request.Set_Parameter ("post", "1");
         Request.Set_Parameter ("post-title", "Post title");
         Request.Set_Parameter ("text", "The blog post content.");
         Request.Set_Parameter ("uri", "the-blog-url");
         Request.Set_Parameter ("save", "1");
         Request.Set_Parameter ("post-status", "1");
         Request.Set_Parameter ("allow-comment", "0");
         ASF.Tests.Do_Post (Request, Reply, "/blogs/admin/create.html", "create-post.html");

         T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY, "Invalid response");
         ASF.Tests.Assert_Redirect (T, "/asfunit/blogs/admin/list.html?blog_id="
                                    & R (Pos + 1 .. R'Last),
                                    Reply, "Invalid redirect after blog post creation");
      end;

   end Test_Create_Blog;

end AWA.Blogs.Tests;
