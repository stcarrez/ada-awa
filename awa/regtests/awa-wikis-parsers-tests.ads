-----------------------------------------------------------------------
--  awa-wikis-parsers-tests -- Unit tests for wiki parsing
--  Copyright (C) 2011 Stephane Carrez
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

with AUnit.Test_Suites;
with Util.Tests;

package AWA.Wikis.Parsers.Tests is

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test bold rendering.
   procedure Test_Wiki_Bold (T : in out Test);

   --  Test italic rendering.
   procedure Test_Wiki_Italic (T : in out Test);

   --  Test various format rendering.
   procedure Test_Wiki_Formats (T : in out Test);

   --  Test heading rendering.
   procedure Test_Wiki_Section (T : in out Test);

   --  Test list rendering.
   procedure Test_Wiki_List (T : in out Test);

   --  Test link rendering.
   procedure Test_Wiki_Link (T : in out Test);

   --  Test quote rendering.
   procedure Test_Wiki_Quote (T : in out Test);

   --  Test line break rendering.
   procedure Test_Wiki_Line_Break (T : in out Test);

   --  Test image rendering.
   procedure Test_Wiki_Image (T : in out Test);

   --  Test preformatted rendering.
   procedure Test_Wiki_Preformatted (T : in out Test);

end AWA.Wikis.Parsers.Tests;
