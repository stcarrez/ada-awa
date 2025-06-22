-----------------------------------------------------------------------
--  awa-counters-modules-tests -- Unit tests for counters service
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Counters.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Manager : AWA.Counters.Modules.Counter_Module_Access;
   end record;

   --  Test incrementing counters and flushing.
   procedure Test_Increment (T : in out Test);

   --  Test incrementing a global counter.
   procedure Test_Global_Counter (T : in out Test);

end AWA.Counters.Modules.Tests;
