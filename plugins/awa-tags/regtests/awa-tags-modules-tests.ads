-----------------------------------------------------------------------
--  awa-tags-modules-tests -- Unit tests for tags service
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Tags.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Manager : AWA.Tags.Modules.Tag_Module_Access;
   end record;

   --  Test tag creation.
   procedure Test_Add_Tag (T : in out Test);

   --  Test tag removal.
   procedure Test_Remove_Tag (T : in out Test);

   --  Test tag creation and removal.
   procedure Test_Update_Tag (T : in out Test);

end AWA.Tags.Modules.Tests;
