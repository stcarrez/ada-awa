-----------------------------------------------------------------------
--  awa-mail-module-tests -- Unit tests for Mail module
--  Copyright (C) 2012, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Mail.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   --  Create an email message with the given template and verify its content.
   procedure Test_Mail_Message (T    : in out Test;
                                Name : in String);

   --  Create an email message and verify its content.
   procedure Test_Create_Message (T : in out Test);

   --  Create an email message with Cc: and verify its content.
   procedure Test_Cc_Message (T : in out Test);

   --  Create an email message with Bcc: and verify its content.
   procedure Test_Bcc_Message (T : in out Test);

end AWA.Mail.Modules.Tests;
