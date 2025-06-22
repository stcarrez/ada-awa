-----------------------------------------------------------------------
--  awa-images-tests -- Unit tests for images module
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;
with Ada.Strings.Unbounded;

package AWA.Images.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Folder_Ident  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Get some access on the image as anonymous users.
   procedure Verify_Anonymous (T     : in out Test;
                               Page  : in String;
                               Title : in String);

   --  Verify that the image lists contain the given image.
   procedure Verify_List_Contains (T    : in out Test;
                                   Name : in String);

   --  Test access to the image as anonymous user.
   procedure Test_Anonymous_Access (T : in out Test);

   --  Test creation of image by simulating web requests.
   procedure Test_Create_Image (T : in out Test);

   --  Test getting an image which does not exist.
   procedure Test_Missing_Image (T : in out Test);

end AWA.Images.Tests;
