-----------------------------------------------------------------------
--  jobs-modules-tests -- Unit tests for AWA job module
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Jobs.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   --  Test the job factory.
   procedure Test_Register (T : in out Test);

end AWA.Jobs.Modules.Tests;
