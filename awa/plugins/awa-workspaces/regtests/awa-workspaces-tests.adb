-----------------------------------------------------------------------
--  awa-workspaces-tests -- Unit tests for workspaces and invitations
--  Copyright (C) 2017 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Util.Test_Caller;
with Util.Strings;
with ADO;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Principals;
with ASF.Helpers.Beans;
with ASF.Tests;
with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
with Security.Contexts;
with AWA.Workspaces.Beans;

package body AWA.Workspaces.Tests is

   use Ada.Strings.Unbounded;
   use AWA.Tests;

   function Get_Invitation_Bean is
     new ASF.Helpers.Beans.Get_Request_Bean (Element_Type   => Beans.Invitation_Bean,
                                             Element_Access => Beans.Invitation_Bean_Access);

   package Caller is new Util.Test_Caller (Test, "Workspaces.Beans");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Workspaces.Beans.Send",
                       Test_Invite_User'Access);
   end Add_Tests;

   --  ------------------------------
   --  Get some access on the blog as anonymous users.
   --  ------------------------------
   procedure Verify_Anonymous (T    : in out Test;
                               Key  : in String) is
      Request   : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
   begin
      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation.html", "invitation-view.html");
      ASF.Tests.Assert_Contains (T, "Blog posts", Reply, "This invitation is invalid");

      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation.html?key=test", "invitation-bad.html");
      ASF.Tests.Assert_Contains (T, "Tag - test", Reply, "Blog tag page is invalid");

      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation.html?key=x" & key,
                        "invitation-bad2.html");
      ASF.Tests.Assert_Contains (T, "The post you are looking for does not exist",
                                 Reply, "Blog post missing page is invalid");

      if Key = "" then
         return;
      end if;

      ASF.Tests.Do_Get (Request, Reply, "/auth/invitation.html?key=" & Key, "invitation-ok.html");
      ASF.Tests.Assert_Contains (T, "post-title", Reply, "Blog post page is invalid");

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
      Check     : AWA.Workspaces.Beans.Invitation_Bean;
      Key       : AWA.Users.Models.Access_Key_Ref;
      Outcome   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      AWA.Tests.Helpers.Users.Login ("test-invite@test.com", Request);
      Request.Set_Parameter ("email", "invited-user@test.com");
      Request.Set_Parameter ("message", "I invite you to this application");
      Request.Set_Parameter ("send", "1");
      Request.Set_Parameter ("invite", "1");
      ASF.Tests.Do_Post (Request, Reply, "/workspaces/invite.html", "invite.html");

      T.Assert (Reply.Get_Status = ASF.Responses.SC_OK,
                "Invalid response after invitation creation");

      --  Verify the invitation by looking at the inviteUser bean.
      Invite := Get_Invitation_Bean (Request, "inviteUser");
      T.Assert (Invite /= null, "Null inviteUser bean");
      T.Assert (Invite.Get_Id /= ADO.NO_IDENTIFIER, "The invite ID is invalid");
      T.Assert (not Invite.Get_Access_Key.Is_Null, "The invite access key is null");
      Check.Key := Invite.Get_Access_Key.Get_Access_Key;
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");
      Check.Load (Outcome);
      Util.Tests.Assert_Equals (T, "success", To_String (Outcome), "Invalid invitation key");
   end Test_Invite_User;

end AWA.Workspaces.Tests;
