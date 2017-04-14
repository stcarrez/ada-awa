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
      Member  : AWA.Workspaces.Models.Workspace_Member_Ref;
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

      --  Create the member instance for this user.
      Member.Set_Workspace (WS);
      Member.Set_Member (User);
      Member.Set_Role ("Owner");
      Member.Set_Join_Date (ADO.Nullable_Time '(Is_Null => False, Value => WS.Get_Create_Date));
      Member.Save (Session);

      --  And give full control of the workspace for this user
      AWA.Permissions.Services.Add_Permission (Session => Session,
                                               User    => User.Get_Id,
                                               Entity  => WS);

      Workspace := WS;
   end Get_Workspace;

   --  ------------------------------
   --  Load the invitation from the access key and verify that the key is still valid.
   --  ------------------------------
   procedure Load_Invitation (Module     : in Workspace_Module;
                              Key        : in String;
                              Invitation : in out AWA.Workspaces.Models.Invitation_Ref'Class;
                              Inviter    : in out AWA.Users.Models.User_Ref) is
      use type Ada.Calendar.Time;

      Ctx    : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query  : ADO.SQL.Query;
      DB_Key : AWA.Users.Models.Access_Key_Ref;
      Found  : Boolean;
   begin
      Log.Debug ("Loading invitation from key {0}", Key);

      Query.Set_Filter ("o.access_key = :key");
      Query.Bind_Param ("key", Key);
      DB_Key.Find (DB, Query, Found);
      if not Found then
         Log.Info ("Invitation key {0} does not exist");
         raise Not_Found;
      end if;
      if DB_Key.Get_Expire_Date < Ada.Calendar.Clock then
         Log.Info ("Invitation key {0} has expired");
         raise Not_Found;
      end if;
      Query.Set_Filter ("o.invitee_id = :user");
      Query.Bind_Param ("user", DB_Key.Get_User.Get_Id);
      Invitation.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("Invitation key {0} has been withdawn");
         raise Not_Found;
      end if;
      Inviter := AWA.Users.Models.User_Ref (Invitation.Get_Inviter);
   end Load_Invitation;

   --  ------------------------------
   --  Accept the invitation identified by the access key.
   --  ------------------------------
   procedure Accept_Invitation (Module     : in Workspace_Module;
                                Key        : in String) is
      use type Ada.Calendar.Time;
      use type ADO.Identifier;

      Ctx          : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      User         : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      DB           : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query        : ADO.SQL.Query;
      DB_Key       : AWA.Users.Models.Access_Key_Ref;
      Found        : Boolean;
      Invitation   : AWA.Workspaces.Models.Invitation_Ref;
      Invitee_Id   : ADO.Identifier;
      Workspace_Id : ADO.Identifier;
      Member       : AWA.Workspaces.Models.Workspace_Member_Ref;
      User_Member  : AWA.Workspaces.Models.Workspace_Member_Ref;
      Now          : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Log.Debug ("Accept invitation with key {0}", Key);
      Ctx.Start;

      --  Get the access key and verify its validity.
      Query.Set_Filter ("o.access_key = :key");
      Query.Bind_Param ("key", Key);
      DB_Key.Find (DB, Query, Found);
      if not Found then
         Log.Info ("Invitation key {0} does not exist", Key);
         raise Not_Found;
      end if;
      if DB_Key.Get_Expire_Date < Now then
         Log.Info ("Invitation key {0} has expired", Key);
         raise Not_Found;
      end if;

      --  Find the invitation associated with the access key.
      Invitee_Id := DB_Key.Get_User.Get_Id;
      Query.Set_Filter ("o.invitee_id = :user");
      Query.Bind_Param ("user", Invitee_Id);
      Invitation.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("Invitation key {0} has been withdawn", Key);
         raise Not_Found;
      end if;
      Workspace_Id := Invitation.Get_Workspace.Get_Id;

      --  Update the workspace member relation.
      Query.Clear;
      Query.Set_Filter ("o.member_id = ? AND o.workspace_id = ?");
      Query.Add_Param (Invitee_Id);
      Query.Add_Param (Workspace_Id);
      Member.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("Invitation key {0} has been withdawn", Key);
         raise Not_Found;
      end if;
      Member.Set_Join_Date (ADO.Nullable_Time '(Is_Null => False,
                                                Value   => Now));
      Invitation.Set_Acceptance_Date (ADO.Nullable_Time '(Is_Null => False,
                                                          Value   => Now));

      --  The user who received the invitation is different from the user who is
      --  logged and accepted the validation.   Since the key is verified, this is
      --  the same user but the user who accepted the invitation registered using
      --  another email address.
      if Invitee_Id /= User.Get_Id then
         --  Check whether the user is not already part of the workspace.
         Query.Clear;
         Query.Set_Filter ("o.member_id = ? AND o.workspace_id = ?");
         Query.Add_Param (User.Get_Id);
         Query.Add_Param (Workspace_Id);
         User_Member.Find (DB, Query, Found);
         if Found then
            Member.Delete (DB);
            Invitation.Delete (DB);
            Log.Info ("Invitation accepted by user who is already a member");
         else
            Member.Set_Member (User);
            Log.Info ("Invitation accepted by user with another email address");
         end if;
         Invitation.Set_Invitee (User);
      end if;
      if not Member.Is_Null then
         Member.Save (DB);
      end if;
      DB_Key.Delete (DB);
      if not Invitation.Is_Null then
         Invitation.Save (DB);
      end if;
      Ctx.Commit;
   end Accept_Invitation;

   --  ------------------------------
   --  Send the invitation to the user.
   --  ------------------------------
   procedure Send_Invitation (Module     : in Workspace_Module;
                              Invitation : in out AWA.Workspaces.Models.Invitation_Ref'Class) is
      use type ADO.Identifier;

      Ctx     : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      DB      : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      User    : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      WS      : AWA.Workspaces.Models.Workspace_Ref;
      Query   : ADO.SQL.Query;
      Found   : Boolean;
      Key     : AWA.Users.Models.Access_Key_Ref;
      Email   : AWA.Users.Models.Email_Ref;
      Invitee : AWA.Users.Models.User_Ref;
      Invit   : AWA.Workspaces.Models.Invitation_Ref;
      Member  : AWA.Workspaces.Models.Workspace_Member_Ref;
      Email_Address : constant String := Invitation.Get_Email;
   begin
      Log.Info ("Sending invitation to {0}", Email_Address);

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

      Query.Clear;
      Query.Set_Filter ("o.email = ?");
      Query.Add_Param (Email_Address);
      Email.Find (DB, Query, Found);
      if not Found then
         Email.Set_User_Id (0);
         Email.Set_Email (Email_Address);
         Email.Save (DB);
         Invitee.Set_Email (Email);
         Invitee.Set_Name (Email_Address);
         Invitee.Save (DB);
         Email.Set_User_Id (Invitee.Get_Id);
         Email.Save (DB);
      else
         Invitee.Load (DB, Email.Get_User_Id);
      end if;

      --  Create the workspace member relation.
      Query.Clear;
      Query.Set_Filter ("o.member_id = ? AND o.workspace_id = ?");
      Query.Add_Param (Invitee.Get_Id);
      Query.Add_Param (WS.Get_Id);
      Member.Find (DB, Query, Found);
      if not Found then
         Member.Set_Member (Invitee);
         Member.Set_Workspace (WS);
         Member.Set_Role ("Invited");
         Member.Save (DB);
      end if;

      --  Check for a previous invitation for the user and delete it.
      Query.Set_Filter ("o.invitee_id = ? AND o.workspace_id = ?");
      Invit.Find (DB, Query, Found);
      if Found then
         Key := AWA.Users.Models.Access_Key_Ref (Invit.Get_Access_Key);
         Key.Delete (DB);
         if not Invitation.Is_Inserted or else Invit.Get_Id /= Invitation.Get_Id then
            Invit.Delete (DB);
         end if;
      end if;
      Key := AWA.Users.Models.Access_Key_Ref (Invitation.Get_Access_Key);
      Module.User_Manager.Create_Access_Key (Invitee, Key, 365 * 86400.0, DB);
      Key.Save (DB);
      Invitation.Set_Access_Key (Key);
      Invitation.Set_Inviter (User);
      Invitation.Set_Invitee (Invitee);
      Invitation.Set_Workspace (WS);
      Invitation.Set_Create_Date (Ada.Calendar.Clock);
      Invitation.Save (DB);

      --  Send the email with the reset password key
      declare
         Event : AWA.Events.Module_Event;
      begin
         Event.Set_Parameter ("key", Key.Get_Access_Key);
         Event.Set_Parameter ("email", Email_Address);
         Event.Set_Parameter ("name", Invitee.Get_Name);
         Event.Set_Parameter ("message", Invitation.Get_Message);
         Event.Set_Parameter ("inviter", User.Get_Name);
         Event.Set_Event_Kind (Invite_User_Event.Kind);
         Module.Send_Event (Event);
      end;

      Ctx.Commit;
   end Send_Invitation;

end AWA.Workspaces.Modules;
