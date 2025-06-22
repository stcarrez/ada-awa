-----------------------------------------------------------------------
--  awa-images-modules-tests -- Unit tests for image service
--  Copyright (C) 2012, 2013, 2018, 2020, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;
with ADO;
package AWA.Images.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Id      : ADO.Identifier;
      Manager : AWA.Images.Modules.Image_Module_Access;
   end record;

   --  Test creation of a storage object
   procedure Test_Create_Image (T : in out Test);

   --  Test the Extract_Size internal method.
   procedure Test_Extract_Size (T : in out Test);

   --  Test the Get_Sizes operation.
   procedure Test_Get_Sizes (T : in out Test);

   --  Test the Scale operation.
   procedure Test_Scale (T : in out Test);

   --  Test the creation of an image through the storage service.
   procedure Test_Store_Image (T : in out Test);

end AWA.Images.Modules.Tests;
