-----------------------------------------------------------------------
--  awa-changelogs-tests -- Tests for changelogs
--  Copyright (C) 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Changelogs.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   procedure Test_Add_Log (T : in out Test);

end AWA.Changelogs.Modules.Tests;
