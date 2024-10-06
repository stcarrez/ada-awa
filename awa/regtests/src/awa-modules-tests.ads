-----------------------------------------------------------------------
--  asf-modules-tests - Unit tests for Modules
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with AWA.Applications;
package AWA.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      --  The Application is a controlled object.  Due to AUnit implementation,
      --  we cannot store a controlled object in the Test object.  This is due to
      --  the 'for Obj'Address use Ret;' clause used by AUnit to allocate a test object.
      --  The application object is allocated dyanmically by Set_Up.
      App : AWA.Applications.Application_Access;
   end record;

   --  Initialize the test application
   overriding
   procedure Set_Up (T : in out Test);

   --  Deletes the application object
   overriding
   procedure Tear_Down (T : in out Test);

   --  Test creation of cookie
   procedure Test_Create_Module (T : in out Test);

   --  Test looking for module
   procedure Test_Find_Module (T : in out Test);

   --  Test creation of a module and registration in an application.
   procedure Test_Create_Application_Module (T : in out Test);

   --  Test module and navigation rules
   procedure Test_Module_Navigation (T : in out Test);

end AWA.Modules.Tests;
