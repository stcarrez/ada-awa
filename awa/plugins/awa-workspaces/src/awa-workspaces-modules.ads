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
with AWA.Events;
package AWA.Workspaces.Modules is

   Not_Found : exception;

   --  The name under which the module is registered.
   NAME : constant String := "workspaces";

   package Invite_User_Event is new AWA.Events.Definition (Name => "invite-user");

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
                              Invitation : in out AWA.Workspaces.Models.Invitation_Ref'Class);

   --  Accept the invitation identified by the access key.
   procedure Accept_Invitation (Module     : in Workspace_Module;
                                Key        : in String);

   --  Send the invitation to the user.
   procedure Send_Invitation (Module : in Workspace_Module;
                              Invitation : in out AWA.Workspaces.Models.Invitation_Ref'Class);

private

   type Workspace_Module is new AWA.Modules.Module with record
      User_Manager : AWA.Users.Services.User_Service_Access;
   end record;

end AWA.Workspaces.Modules;
