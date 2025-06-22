-----------------------------------------------------------------------
--  awa-workspaces-module -- Module workspaces
--  Copyright (C) 2011, 2012, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Objects;
with ADO.Sessions;

with Security.Permissions;

with ASF.Applications;

with AWA.Modules;
with AWA.Services.Contexts;
with AWA.Workspaces.Models;
with AWA.Users.Services;
with AWA.Users.Models;
with AWA.Permissions.Services;
with AWA.Events;
private with Ada.Strings.Unbounded;

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

   subtype Permission_Index_Array is Security.Permissions.Permission_Index_Array;

   Not_Found : exception;

   --  The name under which the module is registered.
   NAME : constant String := "workspaces";

   --  The configuration parameter that defines the list of permissions to grant
   --  to a user when his workspace is created.
   PARAM_PERMISSIONS_LIST : constant String := "permissions_list";

   --  A boolean configuration parameter that indicates that new users can create
   --  a workspace.  When a user is created, the 'workspace-create' permission is
   --  added so that they can create the workspace.
   PARAM_ALLOW_WORKSPACE_CREATE : constant String := "allow_workspace_create";

   --  Permission to create a workspace.
   package ACL_Create_Workspace is new Security.Permissions.Definition ("workspace-create");
   package ACL_Invite_User is new Security.Permissions.Definition ("workspace-invite-user");
   package ACL_Delete_User is new Security.Permissions.Definition ("workspace-delete-user");

   package Invite_User_Event is new AWA.Events.Definition (Name => "invite-user");
   package Accept_Invitation_Event is new AWA.Events.Definition (Name => "accept-invitation");

   --  ------------------------------
   --  Module workspaces
   --  ------------------------------
   type Workspace_Module is new AWA.Modules.Module and AWA.Users.Services.Listener with private;
   type Workspace_Module_Access is access all Workspace_Module'Class;

   --  Initialize the workspaces module.
   overriding
   procedure Initialize (Plugin : in out Workspace_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Configures the module after its initialization and after having read its XML configuration.
   overriding
   procedure Configure (Plugin : in out Workspace_Module;
                        Props  : in ASF.Applications.Config);

   --  Get the list of permissions for the workspace owner.
   function Get_Owner_Permissions (Manager : in Workspace_Module) return Permission_Index_Array;

   --  Get the workspace module.
   function Get_Workspace_Module return Workspace_Module_Access;

   --  Get the current workspace associated with the current user.
   --  If the user has not workspace, create one.
   procedure Get_Workspace (Session   : in out ADO.Sessions.Master_Session;
                            Context   : in AWA.Services.Contexts.Service_Context_Access;
                            Workspace : out AWA.Workspaces.Models.Workspace_Ref);

   --  Create a workspace for the user.
   procedure Create_Workspace (Module    : in Workspace_Module;
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

   --  Add a list of permissions for all the users of the workspace that have the appropriate
   --  role.  Workspace members will be able to access the given database entity for the
   --  specified list of permissions.
   procedure Add_Permission (Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Objects.Object_Ref'Class;
                             Workspace  : in ADO.Identifier;
                             List       : in Security.Permissions.Permission_Index_Array);

   --  The `On_Create` procedure is called by `Notify_Create` to notify the creation of the user.
   overriding
   procedure On_Create (Module : in Workspace_Module;
                        User   : in AWA.Users.Models.User_Ref'Class);

   --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the user.
   overriding
   procedure On_Update (Module : in Workspace_Module;
                        User   : in AWA.Users.Models.User_Ref'Class) is null;

   --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion of the user.
   overriding
   procedure On_Delete (Module : in Workspace_Module;
                        User   : in AWA.Users.Models.User_Ref'Class) is null;

private

   type Workspace_Module is new AWA.Modules.Module and AWA.Users.Services.Listener with record
      User_Manager      : AWA.Users.Services.User_Service_Access;

      --  The permission manager.
      Perm_Manager      : AWA.Permissions.Services.Permission_Manager_Access;

      --  The list of permissions to grant to a user who creates the workspace.
      Owner_Permissions : Ada.Strings.Unbounded.Unbounded_String;

      --  When set, allow new users to create a workspace.
      Allow_WS_Create   : Boolean := False;
   end record;

end AWA.Workspaces.Modules;
