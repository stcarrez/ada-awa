-----------------------------------------------------------------------
--  awa-mail-clients-tests -- Unit tests for Mail clients
--  Copyright (C) 2012, 2013, 2020 Stephane Carrez
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
with Ada.Unchecked_Deallocation;

with Util.Test_Caller;
with Util.Properties;

package body AWA.Mail.Clients.Tests is

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => AWA.Mail.Clients.Mail_Manager'Class,
                                      Name   => AWA.Mail.Clients.Mail_Manager_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => AWA.Mail.Clients.Mail_Message'Class,
                                     Name   => AWA.Mail.Clients.Mail_Message_Access);

   package Caller is new Util.Test_Caller (Test, "Mail.Clients");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Mail.Clients.Factory",
                       Test_Factory'Access);
      Caller.Add_Test (Suite, "Test AWA.Mail.Clients.Create_Message",
                       Test_Create_Message'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the mail manager factory.
   --  ------------------------------
   procedure Test_Factory (T : in out Test) is
      M : AWA.Mail.Clients.Mail_Manager_Access;
      P : Util.Properties.Manager;
   begin
      M := AWA.Mail.Clients.Factory ("file", P);
      T.Assert (M /= null, "Factory returned a null mail manager");

      Free (M);
      M := AWA.Mail.Clients.Factory ("something", P);
      T.Assert (M = null, "Factory returned a non null mail manager");
   end Test_Factory;

   --  ------------------------------
   --  Create an email message and verify its content.
   --  ------------------------------
   procedure Test_Create_Message (T : in out Test) is
      procedure Send;

      M   : AWA.Mail.Clients.Mail_Manager_Access;

      procedure Send is
         Msg : AWA.Mail.Clients.Mail_Message_Access := M.Create_Message;
         C   : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Append (C, "Le palais doit etre debarasse des fantaisies humaines.");
         Msg.Set_From (Name    => "Iorek Byrnison", Address => "Iorek.Byrnison@svalbard.com");
         Msg.Add_Recipient (Kind => TO, Name => "Tous les ours", Address => "all@svalbard.com");
         Msg.Set_Subject (Subject => "Decret");
         Msg.Set_Body (Content => C, Alternative => C);
         Msg.Send;
         Free (Msg);
      end Send;

   begin
      M := AWA.Mail.Clients.Factory ("file", Util.Tests.Get_Properties);
      T.Assert (M /= null, "Factory returned a null mail manager");

      for I in 1 .. 10 loop
         Send;
      end loop;
      Free (M);

      --  The SMTP mailer could be disabled from the configuration.
      M := AWA.Mail.Clients.Factory ("smtp", Util.Tests.Get_Properties);
      if M /= null then
         for I in 1 .. 10 loop
            Send;
         end loop;
         Free (M);
      end if;
   end Test_Create_Message;

end AWA.Mail.Clients.Tests;
