-----------------------------------------------------------------------
--  awa-workspaces-module -- Module workspaces
--  Copyright (C) 2011, 2012, 2013, 2017, 2018, 2019, 2020 Stephane Carrez
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

with AWA.Modules.Beans;
with AWA.Modules.Get;

with ADO.SQL;
with ADO.Sessions.Entities;
with ADO.Queries;
with ADO.Statements;
with Security.Policies.Roles;
with Util.Log.Loggers;
with Util.Mail;
with AWA.Users.Modules;
with AWA.Workspaces.Beans;
package body AWA.Workspaces.Modules is

   use type ADO.Identifier;

   package APS renames AWA.Permissions.Services;
   package ASC renames AWA.Services.Contexts;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Workspaces.Module");

   package Register is new AWA.Modules.Beans (Module => Workspace_Module,
                                              Module_Access => Workspace_Module_Access);

   --  ------------------------------
   --  Initialize the workspaces module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Workspace_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
      Sec_Manager : constant Security.Policies.Policy_Manager_Access
         := Plugin.Get_Application.Get_Security_Manager;
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
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Workspaces.Beans.Member_Bean",
                         Handler => AWA.Workspaces.Beans.Create_Member_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      Plugin.User_Manager := AWA.Users.Modules.Get_User_Manager;
      Plugin.Perm_Manager := APS.Permission_Manager'Class (Sec_Manager.all)'Access;
      Plugin.Add_Listener (AWA.Users.Modules.NAME, Plugin'Unchecked_Access);
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Workspace_Module;
                        Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);

      List : constant String := Plugin.Get_Config (PARAM_PERMISSIONS_LIST);
   begin
      Plugin.Owner_Permissions := Ada.Strings.Unbounded.To_Unbounded_String (List);
      Plugin.Allow_WS_Create := Plugin.Get_Config (PARAM_ALLOW_WORKSPACE_CREATE);
   end Configure;

   --  ------------------------------
   --  Get the list of permissions for the workspace owner.
   --  ------------------------------
   function Get_Owner_Permissions (Manager : in Workspace_Module) return Permission_Index_Array is
      use Ada.Strings.Unbounded;
   begin
      return Security.Permissions.Get_Permission_Array (To_String (Manager.Owner_Permissions));
   end Get_Owner_Permissions;

   --  ------------------------------
   --  Get the workspace module.
   --  ------------------------------
   function Get_Workspace_Module return Workspace_Module_Access is
      function Get is new AWA.Modules.Get (Workspace_Module, Workspace_Module_Access, NAME);
   begin
      return Get;
   end Get_Workspace_Module;

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
      Plugin  : constant Workspace_Module_Access := Get_Workspace_Module;
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

      --  Check that the user has the permission to create a new workspace.
      AWA.Permissions.Check (Permission => ACL_Create_Workspace.Permission,
                             Entity     => User);

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
      Add_Permission (Session   => Session,
                      User      => User.Get_Id,
                      Entity    => WS,
                      Workspace => WS.Get_Id,
                      List      => Plugin.Get_Owner_Permissions);

      Workspace := WS;
   end Get_Workspace;

   --  ------------------------------
   --  Create a workspace for the user.
   --  ------------------------------
   procedure Create_Workspace (Module    : in Workspace_Module;
                               Workspace : out AWA.Workspaces.Models.Workspace_Ref) is
      Ctx     : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      DB      : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      WS      : AWA.Workspaces.Models.Workspace_Ref;
      Member  : AWA.Workspaces.Models.Workspace_Member_Ref;
   begin
      if User.Is_Null then
         Log.Error ("There is no current user.  The workspace cannot be identified");
         Workspace := AWA.Workspaces.Models.Null_Workspace;
         return;
      end if;

      --  Check that the user has the permission to create a new workspace.
      AWA.Permissions.Check (Permission => ACL_Create_Workspace.Permission,
                             Entity     => User);

      DB.Begin_Transaction;

      --  Create a workspace for this user.
      WS.Set_Owner (User);
      WS.Set_Create_Date (Ada.Calendar.Clock);
      WS.Save (DB);

      --  Create the member instance for this user.
      Member.Set_Workspace (WS);
      Member.Set_Member (User);
      Member.Set_Role ("Owner");
      Member.Set_Join_Date (ADO.Nullable_Time '(Is_Null => False, Value => WS.Get_Create_Date));
      Member.Save (DB);

      --  And give full control of the workspace for this user
      Add_Permission (Session   => DB,
                      User      => User.Get_Id,
                      Entity    => WS,
                      Workspace => WS.Get_Id,
                      List      => Module.Get_Owner_Permissions);

      Workspace := WS;
      DB.Commit;
   end Create_Workspace;

   --  ------------------------------
   --  Load the invitation from the access key and verify that the key is still valid.
   --  ------------------------------
   procedure Load_Invitation (Module     : in Workspace_Module;
                              Key        : in String;
                              Invitation : in out AWA.Workspaces.Models.Invitation_Ref'Class;
                              Inviter    : in out AWA.Users.Models.User_Ref) is
      pragma Unreferenced (Module);
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
      Member := AWA.Workspaces.Models.Workspace_Member_Ref (Invitation.Get_Member);
      Workspace_Id := Invitation.Get_Workspace.Get_Id;

      --  Update the workspace member relation.
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
            Invitation.Set_Invitee (User);
         end if;
      end if;
      if not Member.Is_Null then
         Member.Save (DB);
      end if;
      DB_Key.Delete (DB);
      if not Invitation.Is_Null then
         Invitation.Save (DB);

         --  Send the accepted invitation event.
         declare
            Event : AWA.Events.Module_Event;
         begin
            Event.Set_Parameter ("invitee_email", User.Get_Email.Get_Email);
            Event.Set_Parameter ("invitee_name", User.Get_Name);
            Event.Set_Parameter ("message", Invitation.Get_Message);
            Event.Set_Parameter ("inviter_email", Invitation.Get_Inviter.Get_Email.Get_Email);
            Event.Set_Parameter ("inviter_name", Invitation.Get_Inviter.Get_Name);
            Event.Set_Event_Kind (Accept_Invitation_Event.Kind);
            Module.Send_Event (Event);
         end;
      end if;

      Ctx.Commit;
   end Accept_Invitation;

   --  ------------------------------
   --  Send the invitation to the user.
   --  ------------------------------
   procedure Send_Invitation (Module     : in Workspace_Module;
                              Invitation : in out AWA.Workspaces.Models.Invitation_Ref'Class) is

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
      Invite_Address : constant String := Invitation.Get_Email;
      Email_Address  : constant Util.Mail.Email_Address
        := Util.Mail.Parse_Address (Invite_Address);
   begin
      Log.Info ("Sending invitation to {0}", Invite_Address);

      if User.Is_Null then
         Log.Error ("There is no current user.  The workspace cannot be identified");
         return;
      end if;

      --  Find the workspace associated with the current user.
      Query.Set_Join ("INNER JOIN awa_workspace_member AS m ON m.workspace_id = o.id");
      Query.Set_Filter ("m.member_id = ?");
      Query.Add_Param (User.Get_Id);
      WS.Find (DB, Query, Found);
      if not Found then
         Log.Error ("The current user has no associated workspace");
         return;
      end if;

      --  Check that the user has the permission to invite users in the workspace.
      AWA.Permissions.Check (Permission => ACL_Invite_User.Permission,
                             Entity     => WS);

      Ctx.Start;
      Query.Clear;
      Query.Set_Filter ("LOWER(o.email) = LOWER(?)");
      Query.Add_Param (Email_Address.Address);
      Email.Find (DB, Query, Found);
      if not Found then
         Email.Set_User_Id (0);
         Email.Set_Email (Email_Address.Address);
         Email.Save (DB);
         Invitee.Set_Email (Email);
         Invitee.Set_Name (Email_Address.Name);
         Invitee.Set_First_Name (Util.Mail.Get_First_Name (Email_Address));
         Invitee.Set_Last_Name (Util.Mail.Get_Last_Name (Email_Address));
         Invitee.Save (DB);
         Email.Set_User_Id (Invitee.Get_Id);
         Email.Save (DB);
      elsif Email.Get_User_Id /= ADO.NO_IDENTIFIER then
         Invitee.Load (DB, Email.Get_User_Id);
      else
         Invitee.Set_Email (Email);
         Invitee.Set_Name (Email_Address.Name);
         Invitee.Set_First_Name (Util.Mail.Get_First_Name (Email_Address));
         Invitee.Set_Last_Name (Util.Mail.Get_Last_Name (Email_Address));
         Invitee.Save (DB);
         Email.Set_User_Id (Invitee.Get_Id);
         Email.Save (DB);
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
      Module.User_Manager.Create_Access_Key (Invitee, Key, AWA.Users.Models.INVITATION_KEY,
                                             365 * 86400.0, DB);
      Key.Save (DB);
      Invitation.Set_Access_Key (Key);
      Invitation.Set_Inviter (User);
      Invitation.Set_Invitee (Invitee);
      Invitation.Set_Workspace (WS);
      Invitation.Set_Create_Date (Ada.Calendar.Clock);
      Invitation.Set_Member (Member);
      Invitation.Save (DB);

      --  Send the email with the reset password key
      declare
         Event : AWA.Events.Module_Event;
      begin
         Event.Set_Parameter ("key", Key.Get_Access_Key);
         Event.Set_Parameter ("email", Ada.Strings.Unbounded.To_String (Email_Address.Address));
         Event.Set_Parameter ("name", Invitee.Get_Name);
         Event.Set_Parameter ("message", Invitation.Get_Message);
         Event.Set_Parameter ("inviter", User.Get_Name);
         Event.Set_Event_Kind (Invite_User_Event.Kind);
         Module.Send_Event (Event);
      end;

      Ctx.Commit;
   end Send_Invitation;

   --  ------------------------------
   --  Delete the member from the workspace.  Remove the invitation if there is one.
   --  ------------------------------
   procedure Delete_Member (Module       : in Workspace_Module;
                            Member_Id    : in ADO.Identifier) is
      pragma Unreferenced (Module);

      Ctx          : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      DB           : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      User         : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      Query        : ADO.SQL.Query;
      Found        : Boolean;
      Key          : AWA.Users.Models.Access_Key_Ref;
      Member       : AWA.Workspaces.Models.Workspace_Member_Ref;
      Invitation   : AWA.Workspaces.Models.Invitation_Ref;
      User_Id      : ADO.Identifier;
      Workspace_Id : ADO.Identifier;
      User_Image   : constant String := ADO.Identifier'Image (Member_Id);
   begin
      Log.Info ("Delete user member {0}", User_Image);

      --  Get the workspace member instance for the user and remove it.
      Member.Load (DB, Member_Id, Found);
      if not Found then
         Log.Error ("User member {0} does not exist", User_Image);
         return;
      end if;

      User_Id := Member.Get_Member.Get_Id;
      Workspace_Id := Member.Get_Workspace.Get_Id;

      if User.Get_Id = User_Id then
         Log.Warn ("Refusing to delete the current user {0}", User_Image);
         return;
      end if;

      --  Check that the user has the permission to delete users from the workspace.
      AWA.Permissions.Check (Permission => ACL_Delete_User.Permission,
                             Entity     => Workspace_Id);

      Ctx.Start;
      Member.Delete (DB);

      --  Get the invitation and remove it.
      Query.Set_Filter ("o.member_id = ?");
      Query.Add_Param (Member_Id);
      Invitation.Find (DB, Query, Found);
      if Found then
         Key := AWA.Users.Models.Access_Key_Ref (Invitation.Get_Access_Key);
         Key.Delete (DB);
         Invitation.Delete (DB);
      end if;

      --  Remove all permissions assigned to the user in the workspace.
      AWA.Permissions.Services.Delete_Permissions (DB, User_Id, Workspace_Id);
      Ctx.Commit;
   end Delete_Member;

   --  ------------------------------
   --  Add a list of permissions for all the users of the workspace that have the appropriate
   --  role.  Workspace members will be able to access the given database entity for the
   --  specified list of permissions.
   --  ------------------------------
   procedure Add_Permission (Session    : in out ADO.Sessions.Master_Session;
                             User       : in ADO.Identifier;
                             Entity     : in ADO.Objects.Object_Ref'Class;
                             Workspace  : in ADO.Identifier;
                             List       : in Security.Permissions.Permission_Index_Array) is
      Ctx  : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
      Key  : constant ADO.Objects.Object_Key := Entity.Get_Key;
      Id   : constant ADO.Identifier := ADO.Objects.Get_Value (Key);
      Kind : constant ADO.Entity_Type
        := ADO.Sessions.Entities.Find_Entity_Type (Session => Session,
                                                   Object  => Key);
      Manager : constant AWA.Permissions.Services.Permission_Manager_Access
        := AWA.Permissions.Services.Get_Permission_Manager (Ctx);
   begin
      for Perm of List loop
         declare
            Member   : ADO.Identifier;
            Query    : ADO.Queries.Context;
            Names    : constant Security.Policies.Roles.Role_Name_Array
              := Manager.Get_Role_Names (Perm);
            Need_Sep : Boolean := False;
            User_Added : Boolean := False;
         begin
            if Names'Length > 0 then
               Query.Set_Query (AWA.Workspaces.Models.Query_Member_In_Role);
               ADO.SQL.Append (Query.Filter, "user_member.workspace_id = :workspace_id");
               ADO.SQL.Append (Query.Filter, " AND user_member.role IN (");
               for Name of Names loop
                  ADO.SQL.Append (Query.Filter, (if Need_Sep then ",?" else "?"));
                  Query.Add_Param (Name.all);
                  Need_Sep := True;
               end loop;
               Query.Bind_Param ("workspace_id", Workspace);
               ADO.SQL.Append (Query.Filter, ")");
               declare
                  Stmt : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
               begin
                  Stmt.Execute;
                  while Stmt.Has_Elements loop
                     Member := Stmt.Get_Identifier (0);
                     if Member = User then
                        User_Added := True;
                     end if;
                     Manager.Add_Permission (Session    => Session,
                                             User       => Member,
                                             Entity     => Id,
                                             Kind       => Kind,
                                             Workspace  => Workspace,
                                             Permission => Perm);
                     Stmt.Next;
                  end loop;
               end;
            end if;
            if not User_Added then
               Manager.Add_Permission (Session    => Session,
                                       User       => User,
                                       Entity     => Id,
                                       Kind       => Kind,
                                       Workspace  => Workspace,
                                       Permission => Perm);
            end if;
         end;
      end loop;
   end Add_Permission;

   --  ------------------------------
   --  The `On_Create` procedure is called by `Notify_Create` to notify the creation of the user.
   --  ------------------------------
   overriding
   procedure On_Create (Module : in Workspace_Module;
                        User   : in AWA.Users.Models.User_Ref'Class) is
      Ctx      : constant ASC.Service_Context_Access := ASC.Current;
      Kind     : ADO.Entity_Type;
   begin
      if Module.Allow_WS_Create then
         declare
            DB       : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
         begin
            Ctx.Start;
            Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, Models.WORKSPACE_TABLE);
            Module.Perm_Manager.Add_Permission (Session    => DB,
                                                User       => User.Get_Id,
                                                Entity     => ADO.NO_IDENTIFIER,
                                                Kind       => Kind,
                                                Workspace  => ADO.NO_IDENTIFIER,
                                                Permission => ACL_Create_Workspace.Permission);
            Ctx.Commit;
         end;
      end if;
   end On_Create;

end AWA.Workspaces.Modules;
