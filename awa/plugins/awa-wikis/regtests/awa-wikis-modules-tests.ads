-----------------------------------------------------------------------
--  awa-wikis-modules-tests -- Unit tests for wikis service
--  Copyright (C) 2015, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;
with ADO;

package AWA.Wikis.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Manager    : AWA.Wikis.Modules.Wiki_Module_Access;
      Wiki_Id    : ADO.Identifier := ADO.NO_IDENTIFIER;
      Public_Id  : ADO.Identifier := ADO.NO_IDENTIFIER;
      Private_Id : ADO.Identifier := ADO.NO_IDENTIFIER;
   end record;

   --  Test creation of a wiki space.
   procedure Test_Create_Wiki_Space (T : in out Test);

   --  Test creation of a wiki page.
   procedure Test_Create_Wiki_Page (T : in out Test);

   --  Test creation of a wiki page content.
   procedure Test_Create_Wiki_Content (T : in out Test);

   --  Test getting the wiki page as well as info, history pages.
   procedure Test_Wiki_Page (T : in out Test);

   --  Test updating the wiki page through a POST request.
   procedure Test_Update_Page (T : in out Test);

end AWA.Wikis.Modules.Tests;
