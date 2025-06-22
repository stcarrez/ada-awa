-----------------------------------------------------------------------
--  awa-workspaces-tests -- Unit tests for workspaces and invitations
--  Copyright (C) 2017, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Util.Tests;
with ADO;
with AWA.Tests;

package AWA.Workspaces.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Member_Id : ADO.Identifier;
      Invite_Id : ADO.Identifier;
      Key       : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Verify the anonymous access for the invitation page.
   procedure Verify_Anonymous (T    : in out Test;
                               Key  : in String);

   --  Test sending an invitation.
   procedure Test_Invite_User (T : in out Test);

   --  Test deleting the member.
   procedure Test_Delete_Member (T : in out Test);

   --  Test accepting the invitation.
   procedure Test_Accept_Invitation (T : in out Test);

   --  Test accepting the invitation with a email and password process.
   procedure Test_Accept_Invitation_With_Email (T : in out Test);

   --  Test listing the members of the workspace.
   procedure Test_List_Members (T : in out Test);

end AWA.Workspaces.Tests;
