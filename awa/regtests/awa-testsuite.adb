-----------------------------------------------------------------------
--  Util testsuite - Util Testsuite
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with AWA.Users.Services.Tests;
with AWA.Users.Tests;
with AWA.Blogs.Services.Tests;
with AWA.Wikis.Parsers.Tests;
with AWA.Helpers.Selectors.Tests;
package body AWA.Testsuite is

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := new Util.Tests.Test_Suite;
   begin
      AWA.Users.Services.Tests.Add_Tests (Ret);
      AWA.Users.Tests.Add_Tests (Ret);
      AWA.Wikis.Parsers.Tests.Add_Tests (Ret);
      AWA.Helpers.Selectors.Tests.Add_Tests (Ret);
      AWA.Blogs.Services.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end AWA.Testsuite;
