-----------------------------------------------------------------------
--  awa-helpers-selectors-tests -- Unit tests for selector helpers
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package AWA.Helpers.Selectors.Tests is

   type Color is (White, Blue, Red, Green, Yellow, Black);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test creation of selector from an SQL query
   procedure Test_Create_From_Query (T : in out Test);

   --  Test creation of selector from an enum definition
   procedure Test_Create_From_Enum (T : in out Test);

end AWA.Helpers.Selectors.Tests;
