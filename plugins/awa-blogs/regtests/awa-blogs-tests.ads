-----------------------------------------------------------------------
--  awa-blogs-tests -- Unit tests for blogs module
--  Copyright (C) 2017, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;
with Ada.Strings.Unbounded;

package AWA.Blogs.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Blog_Ident : Ada.Strings.Unbounded.Unbounded_String;
      Post_Ident : Ada.Strings.Unbounded.Unbounded_String;
      Post_Uri   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Get some access on the blog as anonymous users.
   procedure Verify_Anonymous (T    : in out Test;
                               Post : in String);

   --  Test access to the blog as anonymous user.
   procedure Test_Anonymous_Access (T : in out Test);

   --  Test creation of blog by simulating web requests.
   procedure Test_Create_Blog (T : in out Test);

   --  Test updating a post by simulating web requests.
   procedure Test_Update_Post (T : in out Test);

   --  Test updating the publication date by simulating web requests.
   procedure Test_Update_Publish_Date (T : in out Test);

   --  Test listing the blog posts.
   procedure Test_Admin_List_Posts (T : in out Test);

   --  Test listing the blog comments.
   procedure Test_Admin_List_Comments (T : in out Test);

   --  Test getting the JSON blog stats (for graphs).
   procedure Test_Admin_Blog_Stats (T : in out Test);

   --  Test getting an image from the blog servlet.
   procedure Test_Image_Blog (T : in out Test);

end AWA.Blogs.Tests;
