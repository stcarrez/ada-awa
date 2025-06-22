-----------------------------------------------------------------------
--  awa-workspaces-tests -- Unit tests for workspaces and invitations
--  Copyright (C) 2017, 2018, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Test_Caller;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Helpers.Beans;
with ASF.Tests;
with ADO.Sessions;
with AWA.Users.Models;
with AWA.Tests.Helpers.Users;
with AWA.Workspaces.Beans;
with AWA.Workspaces.Models;
with AWA.Workspaces.Modules;

package body AWA.Workspaces.Tests is

   use Ada.Strings.Unbounded;

   function Get_Invitation_Bean is
     new ASF.Helpers.Beans.Get_Request_Bean (Element_Type   => Beans.Invitation_Bean,
                                             Element_Access => Beans.Invitation_Bean_Access);

   package Caller is new Util.Test_Caller (Test, "Workspaces.Beans");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Workspaces.Beans.Send",
                       Test_Invite_User'Access);
      Caller.Add_Test (Suite, "Test AWA.Workspaces.Beans.Delete",
                       Test_Delete_Member'Access);
      Caller.Add_Test (Suite, "Test AWA.Workspaces.Beans.Accept",
                       Test_Accept_Invitation'Access);
      Caller.Add_Test (Suite, "Test AWA.Workspaces.Beans.Accept (with Email)",
                       Test_Accept_Invitation_With_Email'Access);
      Caller.Add_Test (Suite, "Test AWA.Workspaces.Beans.Member_List",
                       Test_List_Members'Access);
   end Add_Tests;

   --  ------------------------------
   --  Verify the anonymous access for the invitation page.
   --  ------------------------------
   procedure Verify_Anonymous (T    : in out Test;
                               Key  : in String) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation.html", "invitation-view.html");
      ASF.Tests.Assert_Contains (T, "Bad or invalid invitation", Reply,
                                 "This invitation is invalid");

      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation/test", "invitation-bad.html");
      ASF.Tests.Assert_Contains (T, "This invitation is invalid or has expired", Reply,
                                 "This invitation is invalid (key)");

      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation/x" & key,
                        "invitation-bad2.html");
      ASF.Tests.Assert_Contains (T, "This invitation is invalid or has expired",
                                 Reply, "This invitation is invalid (key)");

      if Key = "" then
         return;
      end if;

      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation/" & Key, "invitation-ok.html");
      ASF.Tests.Assert_Contains (T, "Accept invitation", Reply,
                                 "Accept invitation page is invalid");

   end Verify_Anonymous;

   --  ------------------------------
   --  Test sending an invitation.
   --  ------------------------------
   procedure Test_Invite_User (T : in out Test) is
      use type ADO.Identifier;
      use type AWA.Workspaces.Beans.Invitation_Bean_Access;

      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
      Invite    : AWA.Workspaces.Beans.Invitation_Bean_Access;
   begin
      AWA.Tests.Helpers.Users.Login ("test-invite@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/workspaces/invite.html", "invite-get.html");

      Request.Set_Parameter ("email", "invited-user@test.com");
      Request.Set_Parameter ("message", "I invite you to this application");
      Request.Set_Parameter ("send", "1");
      ASF.Tests.Set_CSRF (Request, "invite", "invite-get.html");
      ASF.Tests.Do_Post (Request, Reply, "/workspaces/invite.html", "invite.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY,
                "Invalid response after invitation creation");

      --  Verify the invitation by looking at the inviteUser bean.
      Invite := Get_Invitation_Bean (Request, "inviteUser");
      T.Assert (Invite /= null, "Null inviteUser bean");
      T.Assert (Invite.Get_Id /= ADO.NO_IDENTIFIER, "The invite ID is invalid");
      T.Assert (not Invite.Get_Access_Key.Is_Null, "The invite access key is null");
      T.Assert (Invite.Get_Member.Is_Inserted, "The invitation has a workspace member");
      T.Key := Invite.Get_Access_Key.Get_Access_Key;
      T.Invite_Id := Invite.Get_Id;
      T.Verify_Anonymous (Invite.Get_Access_Key.Get_Access_Key);

      T.Member_ID := Invite.Get_Member.Get_Id;
   end Test_Invite_User;

   --  ------------------------------
   --  Test deleting the member.
   --  ------------------------------
   procedure Test_Delete_Member (T : in out Test) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      T.Test_Invite_User;
      AWA.Tests.Helpers.Users.Login ("test-invite@test.com", Request);
      declare
         Id : constant String := ADO.Identifier'Image (T.Member_Id);
      begin
         ASF.Tests.Do_Get (Request, Reply, "/workspaces/forms/delete-member.html",
                           "delete-member-get.html");

         Request.Set_Parameter ("member-id", Id);
         Request.Set_Parameter ("delete", "1");
         ASF.Tests.Set_CSRF (Request, "delete-member-form", "delete-member-get.html");
         ASF.Tests.Do_Post (Request, Reply, "/workspaces/forms/delete-member.html",
                            "delete-member.html");

         T.Assert (Reply.Get_Status = ASF.Responses.SC_OK,
                   "Invalid response after delete member operation");
         ASF.Tests.Assert_Contains (T, "deleteDialog_" & Id (Id'First + 1 .. Id'Last), Reply,
                                    "Delete member dialog operation response is invalid");
      end;
   end Test_Delete_Member;

   --  ------------------------------
   --  Test accepting the invitation.
   --  ------------------------------
   procedure Test_Accept_Invitation (T : in out Test) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      T.Test_Invite_User;
      AWA.Tests.Helpers.Users.Recover_Password ("invited-user@test.com");
      AWA.Tests.Helpers.Users.Login ("invited-user@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation/"
                        & To_String (T.Key),
                        "accept-member.html");
      T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY,
                "Accept invitation page failed");
      Util.Tests.Assert_Equals (T, "/asfunit/workspaces/main.html", Reply.Get_Header ("Location"),
                                "The accept invitation page must redirect to the workspace");
   end Test_Accept_Invitation;

   --  ------------------------------
   --  Test accepting the invitation with a email and password process.
   --  ------------------------------
   procedure Test_Accept_Invitation_With_Email (T : in out Test) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      T.Test_Invite_User;
      Request.Set_Parameter ("key", To_String (T.Key));
      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation/"
                        & To_String (T.Key),
                        "accept-invitation-email-1.html");
      ASF.Tests.Assert_Contains (T, "input type=""hidden"" name=""key"" value="""
                                 & To_String (T.Key),
                                 Reply,
                                 "The invitation form must setup the invitation key form");

      Request.Set_Parameter ("key", To_String (T.Key));
      Request.Set_Parameter ("register-button", "1");
      Request.Set_Parameter ("firstName", "Invitee");
      Request.Set_Parameter ("lastName", "With_Email");
      Request.Set_Parameter ("email", "accept-invitation@test.com");
      Request.Set_Parameter ("email", "invited-user@test.com");
      Request.Set_Parameter ("password", "admin");
      Request.Set_Parameter ("password2", "admin");
      ASF.Tests.Set_CSRF (Request, "register", "accept-invitation-email-1.html");

      ASF.Tests.Do_Post (Request, Reply, "/auth/invitation.html",
                         "accept-invitation-email-2.html");
      T.Assert (Reply.Get_Status = ASF.Responses.SC_MOVED_TEMPORARILY,
                "Accept invitation page failed");
      Util.Tests.Assert_Equals (T, "/asfunit/workspaces/main.html",
                                Reply.Get_Header ("Location"),
                                "The accept invitation page must redirect to the workspace");

      --  Verify that the invitation is accepted.
      declare
         Principal  : AWA.Tests.Helpers.Users.Test_User;
         Module     : AWA.Workspaces.Modules.Workspace_Module_Access;
         DB         : ADO.Sessions.Session;
         Invitation : AWA.Workspaces.Models.Invitation_Ref;
         Found      : Boolean;
      begin
         AWA.Tests.Helpers.Users.Initialize (Principal);

         Module := AWA.Workspaces.Modules.Get_Workspace_Module;
         DB := Module.Get_Session;
         Invitation.Load (DB, T.Invite_Id, Found);
         T.Assert (Found, "Invitation was not found");
         T.Assert (not Invitation.Get_Acceptance_Date.Is_Null,
                   "Invitation not accepted");
      end;
   end Test_Accept_Invitation_With_Email;

   --  ------------------------------
   --  Test listing the members of the workspace.
   --  ------------------------------
   procedure Test_List_Members (T : in out Test) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      T.Test_Invite_User;
      AWA.Tests.Helpers.Users.Login ("test-invite@test.com", Request);
      ASF.Tests.Do_Get (Request, Reply, "/workspaces/members.html",
                        "member-list.html");
      ASF.Tests.Assert_Contains (T, "invited-user@test.com", Reply,
                                 "The invited user is listed in the members page");
      ASF.Tests.Assert_Contains (T, "test-invite@test.com", Reply,
                                 "The invite user (owner) is listed in the members page");
   end Test_List_Members;

end AWA.Workspaces.Tests;
