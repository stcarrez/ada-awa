-----------------------------------------------------------------------
--  awa-blogs-tests -- Unit tests for blogs module
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Blogs.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   --  Test creation of blog by simulating web requests.
   procedure Test_Create_Blog (T : in out Test);

   --  Test creating and updating of a blog post
   procedure Test_Create_Post (T : in out Test);

end AWA.Blogs.Modules.Tests;
