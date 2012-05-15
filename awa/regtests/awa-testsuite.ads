-----------------------------------------------------------------------
--  AWA testsuite - AWA Testsuite
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

with Util.Tests;
with Util.Properties;

with AWA.Applications;

package AWA.Testsuite is

   function Suite return Util.Tests.Access_Test_Suite;

   procedure Initialize (Props : in Util.Properties.Manager);

   --  Initialize the AWA test framework mockup.
   procedure Initialize (App         : in AWA.Applications.Application_Access;
                         Props       : in Util.Properties.Manager;
                         Add_Modules : in Boolean);

end AWA.Testsuite;
