-----------------------------------------------------------------------
--  awa-mail-module-tests -- Unit tests for Mail module
--  Copyright (C) 2012, 2017, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;

with AWA.Events;

package body AWA.Mail.Modules.Tests is

   package Caller is new Util.Test_Caller (Test, "Mail.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Mail.Module.Create_Message",
                       Test_Create_Message'Access);
      Caller.Add_Test (Suite, "Test AWA.Mail.Module.Create_Message (CC:)",
                       Test_Cc_Message'Access);
      Caller.Add_Test (Suite, "Test AWA.Mail.Module.Create_Message (BCC:)",
                       Test_Bcc_Message'Access);
   end Add_Tests;

   --  ------------------------------
   --  Create an email message with the given template and verify its content.
   --  ------------------------------
   procedure Test_Mail_Message (T    : in out Test;
                                Name : in String) is
      use Util.Beans.Objects;

      Mail  : constant AWA.Mail.Modules.Mail_Module_Access := AWA.Mail.Modules.Get_Mail_Module;
      Event : AWA.Events.Module_Event;
      Props : Util.Beans.Objects.Maps.Map;
   begin
      T.Assert (Mail /= null, "There is no current mail module");

      Props.Insert ("name", To_Object (String '("joe")));
      Props.Insert ("email", To_Object (String '("joe.rogers@somewhere.org")));
      Mail.Send_Mail (Template => Name,
                      Props    => Props,
                      Params   => Props,
                      Content  => Event);

   end Test_Mail_Message;

   --  ------------------------------
   --  Create an email message and verify its content.
   --  ------------------------------
   procedure Test_Create_Message (T : in out Test) is
   begin
      T.Test_Mail_Message ("mail-info.html");
   end Test_Create_Message;

   --  ------------------------------
   --  Create an email message with Cc: and verify its content.
   --  ------------------------------
   procedure Test_Cc_Message (T : in out Test) is
   begin
      T.Test_Mail_Message ("mail-cc.html");
   end Test_Cc_Message;

   --  ------------------------------
   --  Create an email message with Bcc: and verify its content.
   --  ------------------------------
   procedure Test_Bcc_Message (T : in out Test) is
   begin
      T.Test_Mail_Message ("mail-bcc.html");
   end Test_Bcc_Message;

end AWA.Mail.Modules.Tests;
