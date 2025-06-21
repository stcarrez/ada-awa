-----------------------------------------------------------------------
--  asf-modules-tests - Unit tests for Modules
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
