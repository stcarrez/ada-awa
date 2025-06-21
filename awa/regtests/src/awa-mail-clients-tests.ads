-----------------------------------------------------------------------
--  awa-mail-clients-tests -- Unit tests for Mail clients
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Mail.Clients.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   --  Test the mail manager factory.
   procedure Test_Factory (T : in out Test);

   --  Create an email message and verify its content.
   procedure Test_Create_Message (T : in out Test);

end AWA.Mail.Clients.Tests;
