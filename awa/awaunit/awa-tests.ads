-----------------------------------------------------------------------
--  AWA tests - AWA Tests Framework
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

with AWA.Applications;

with Util.Properties;
with Util.Tests;
with Util.XUnit;
package AWA.Tests is

   type Test is abstract new Util.Tests.Test with null record;

   --  Setup the service context before executing the test.
   overriding
   procedure Set_Up (T : in out Test);

   --  Initialize the AWA test framework mockup.
   procedure Initialize (App         : in AWA.Applications.Application_Access;
                         Props       : in Util.Properties.Manager;
                         Add_Modules : in Boolean);

   --  Called when the testsuite execution has finished.
   procedure Finish (Status : in Util.XUnit.Status);

   procedure Initialize (Props : in Util.Properties.Manager);

   --  Get the test application.
   function Get_Application return AWA.Applications.Application_Access;

   --  Set the application context to simulate a web request context.
   procedure Set_Application_Context;

end AWA.Tests;
