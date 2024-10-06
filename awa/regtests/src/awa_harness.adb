-----------------------------------------------------------------------
--  AWA - Unit tests
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with AWA.Testsuite;

with Util.Tests;
with AWA.Tests;
procedure AWA_Harness is

   procedure Harness is new Util.Tests.Harness (AWA.Testsuite.Suite,
                                                AWA.Testsuite.Initialize,
                                                AWA.Tests.Finish);

begin
   Harness ("awa-tests.xml");
end AWA_Harness;
