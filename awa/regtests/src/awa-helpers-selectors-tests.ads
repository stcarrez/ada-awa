-----------------------------------------------------------------------
--  awa-helpers-selectors-tests -- Unit tests for selector helpers
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
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
