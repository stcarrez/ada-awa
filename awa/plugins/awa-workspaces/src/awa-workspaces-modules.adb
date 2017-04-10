-----------------------------------------------------------------------
--  awa-workspaces-module -- Module workspaces
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
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

with Ada.Calendar;

with AWA.Users.Models;
with AWA.Modules.Beans;
with AWA.Permissions.Services;

with ADO.SQL;
with Util.Log.Loggers;
with AWA.Users.Modules;
with AWA.Workspaces.Beans;
package body AWA.Workspaces.Modules is

   package ASC renames AWA.Services.Contexts;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Workspaces.Module");

   package Register is new AWA.Modules.Beans (Module => Workspace_Module,
                                              Module_Access => Workspace_Module_Access);

   --  ------------------------------
   --  Initialize the workspaces module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Workspace_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the workspaces module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Workspaces.Beans.Workspaces_Bean",
                         Handler => AWA.Workspaces.Beans.Create_Workspaces_Bean'Access);
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Workspaces.Beans.Member_List_Bean",
                         Handler => AWA.Workspaces.Beans.Create_Member_List_Bean'Access);
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Workspaces.Beans.Invitation_Bean",
                         Handler => AWA.Workspaces.Beans.Create_Invitation_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      Plugin.User_Manager := AWA.Users.Modules.Get_User_Manager;
      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the current workspace associated with the current user.
   --  If the user has not workspace, create one.
   --  ------------------------------
   procedure Get_Workspace (Session   : in out ADO.Sessions.Master_Session;
                            Context   : in AWA.Services.Contexts.Service_Context_Access;
                            Workspace : out AWA.Workspaces.Models.Workspace_Ref) is
      User    : constant AWA.Users.Models.User_Ref := Context.Get_User;
      WS      : AWA.Workspaces.Models.Workspace_Ref;
      Query   : ADO.SQL.Query;
      Found   : Boolean;
   begin
      if User.Is_Null then
         Log.Error ("There is no current user.  The workspace cannot be identified");
         Workspace := AWA.Workspaces.Models.Null_Workspace;
         return;
      end if;

      --  Find the workspace associated with the current user.
      Query.Add_Param (User.Get_Id);
      Query.Set_Filter ("o.owner_id = ?");
      WS.Find (Session, Query, Found);
      if Found then
         Workspace := WS;
         return;
      end if;

      --  Create a workspace for this user.
      WS.Set_Owner (User);
      WS.Set_Create_Date (Ada.Calendar.Clock);
      WS.Save (Session);

      --  And give full control of the workspace for this user
      AWA.Permissions.Services.Add_Permission (Session => Session,
                                               User    => User.Get_Id,
                                               Entity  => WS);

      Workspace := WS;
   end Get_Workspace;

   --  ------------------------------
   --  Send the invitation to the user.
   --  ------------------------------
   procedure Send_Invitation (Module     : in Workspace_Module;
                              Invitation : in out AWA.Workspaces.Models.Invitation_Ref'Class) is
      Ctx   : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      User  : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      WS    : AWA.Workspaces.Models.Workspace_Ref;
      Query : ADO.SQL.Query;
      Found : Boolean;
      Key   : AWA.Users.Models.Access_Key_Ref;
   begin
      Log.Info ("Sending invitation to {0}", String '(Invitation.Get_Email));

      Ctx.Start;
      if User.Is_Null then
         Log.Error ("There is no current user.  The workspace cannot be identified");
         return;
      end if;

      --  Find the workspace associated with the current user.
      Query.Add_Param (User.Get_Id);
      Query.Set_Filter ("o.owner_id = ?");
      WS.Find (DB, Query, Found);
      if not Found then
         return;
      end if;
      Key := AWA.Users.Models.Access_Key_Ref (Invitation.Get_Access_Key);
      Module.User_Manager.Create_Access_Key (User, Key, 365 * 86400.0, DB);
      Key.Save (DB);
      Invitation.Set_Access_Key (Key);
      Invitation.Set_Inviter (User);
      Invitation.Set_Workspace (WS);
      Invitation.Set_Create_Date (Ada.Calendar.Clock);
      Invitation.Save (DB);
      Ctx.Commit;
   end Send_Invitation;

end AWA.Workspaces.Modules;
