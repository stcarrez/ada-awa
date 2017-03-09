-----------------------------------------------------------------------
--  awa-mail-module-tests -- Unit tests for Mail module
--  Copyright (C) 2012, 2017 Stephane Carrez
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
