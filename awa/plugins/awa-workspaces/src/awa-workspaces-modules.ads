-----------------------------------------------------------------------
--  awa-workspaces-module -- Module workspaces
--  Copyright (C) 2011, 2012, 2017 Stephane Carrez
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

with ADO.Sessions;

with ASF.Applications;

with AWA.Modules;
with AWA.Services.Contexts;
with AWA.Workspaces.Models;
with AWA.Users.Services;
with AWA.Users.Models;
with AWA.Events;

--  == Events ==
--  The *workspaces* module provides several events that are posted when some action are performed.
--
--  === invite-user ===
--  This event is posted when an invitation is created for a user.  The event can be used to
--  send the associated invitation email to the invitee.  The event contains the following
--  attributes:
--
--  key
--  email
--  name
--  message
--  inviter
--
--  === accept-invitation ===
--  This event is posted when an invitation is accepted by a user.
package AWA.Workspaces.Modules is

   Not_Found : exception;

   --  The name under which the module is registered.
   NAME : constant String := "workspaces";

   package Invite_User_Event is new AWA.Events.Definition (Name => "invite-user");
   package Accept_Invitation_Event is new AWA.Events.Definition (Name => "accept-invitation");

   --  ------------------------------
   --  Module workspaces
   --  ------------------------------
   type Workspace_Module is new AWA.Modules.Module with private;
   type Workspace_Module_Access is access all Workspace_Module'Class;

   --  Initialize the workspaces module.
   overriding
   procedure Initialize (Plugin : in out Workspace_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the current workspace associated with the current user.
   --  If the user has not workspace, create one.
   procedure Get_Workspace (Session   : in out ADO.Sessions.Master_Session;
                            Context   : in AWA.Services.Contexts.Service_Context_Access;
                            Workspace : out AWA.Workspaces.Models.Workspace_Ref);

   --  Load the invitation from the access key and verify that the key is still valid.
   procedure Load_Invitation (Module     : in Workspace_Module;
                              Key        : in String;
                              Invitation : in out AWA.Workspaces.Models.Invitation_Ref'Class;
                              Inviter    : in out AWA.Users.Models.User_Ref);

   --  Accept the invitation identified by the access key.
   procedure Accept_Invitation (Module     : in Workspace_Module;
                                Key        : in String);

   --  Send the invitation to the user.
   procedure Send_Invitation (Module : in Workspace_Module;
                              Invitation : in out AWA.Workspaces.Models.Invitation_Ref'Class);

   --  Delete the member from the workspace.  Remove the invitation if there is one.
   procedure Delete_Member (Module       : in Workspace_Module;
                            Member_Id    : in ADO.Identifier);

private

   type Workspace_Module is new AWA.Modules.Module with record
      User_Manager : AWA.Users.Services.User_Service_Access;
   end record;

end AWA.Workspaces.Modules;
