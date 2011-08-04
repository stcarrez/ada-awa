-----------------------------------------------------------------------
--  awa.users -- User registration, authentication processes
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Ada.Strings;
with Ada.Calendar;
with Ada.Strings.Unbounded;

with Util.Log.Loggers;

with GNAT.SHA1;
with Ada.Numerics.Discrete_Random;

with ADO.SQL;
with ADO.Sessions;
with ADO.Statements;

with AWA.Services.Contexts;
package body AWA.Users.Services is

   use Util.Log;
   use ADO.Statements;
   use ADO.Sessions;
   use Ada.Strings.Unbounded;
   use ADO.SQL;
   use AWA.Services;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Users.Services");

   package Integer_Random is new Ada.Numerics.Discrete_Random (Integer);

   Random_Generator : Integer_Random.Generator;

   function Random return Integer is
   begin
      return Integer_Random.Random (Random_Generator);
   end Random;

   procedure Send_Alert (Model : in User_Service;
                         Name  : in String;
                         User  : in User_Ref'Class;
                         Props : in out ASF.Events.Modules.Module_Event) is
   begin
      Props.Set_Parameter ("first_name", User.Get_First_Name);
      Props.Set_Parameter ("last_name", User.Get_Last_Name);
      Props.Set_Parameter ("name", Name);
      Model.Send_Event (Name, Props);
   end Send_Alert;

   function Create_Key (Number : ADO.Identifier) return String is
   begin
      return GNAT.SHA1.Digest (Integer'Image (Random) & ADO.Identifier'Image (Number));
   end Create_Key;

   --  ------------------------------
   --  Authenticate the user with his OpenID identifier.  The authentication process
   --  was made by an external OpenID provider.  If the user does not yet exists in
   --  the database, a record is created for him.  Create a new session for the user.
   --  The IP address of the connection is saved in the session.
   --  Raises Not_Found exception if the user is not recognized
   --  ------------------------------
   procedure Authenticate (Model    : in User_Service;
                           OpenId   : in String;
                           Email    : in String;
                           Name     : in String;
                           IpAddr   : in String;
                           User     : out User_Ref'Class;
                           Session  : out Session_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query  : ADO.SQL.Query;
      Found  : Boolean;
   begin
      Log.Info ("Authenticated user {0}", Email);

      Ctx.Start;

      --  Find the user registered under the given OpenID identifier.
      Query.Bind_Param (1, OpenId);
      Query.Set_Filter ("o.openid = ?");
      User.Find (DB, Query, Found);
      if not Found then
         Log.Info ("User {0} is not known", Email);
         declare
            E : Email_Ref;
         begin
            E.Set_Email (Email);
            E.Set_User_Id (0);
            E.Save (DB);

            User.Set_Email (E);
            User.Set_Name (Name);
            User.Set_Open_Id (OpenId);
            User.Save (DB);
            E.Set_User_Id (User.Get_Id);
            E.Save (DB);
         end;
      end if;

      --  Create the session
      Session := Session_Ref'Class (Null_Session);
      Session.Set_Start_Date (ADO.Nullable_Time '(Value => Ada.Calendar.Clock, Is_Null => False));
      Session.Set_User_Id (User.Get_Id);
      Session.Set_Ip_Address (IpAddr);
      Session.Save (DB);

      Ctx.Commit;
   end Authenticate;

   --  ------------------------------
   --  Authenticate the user with his email address and his password.
   --  If the user is authenticated, return the user information and
   --  create a new session.  The IP address of the connection is saved
   --  in the session.
   --  Raises Not_Found exception if the user is not recognized
   --  ------------------------------
   procedure Authenticate (Model    : in User_Service;
                           Email    : in String;
                           Password : in String;
                           IpAddr   : in String;
                           User     : out User_Ref'Class;
                           Session  : out Session_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query  : ADO.SQL.Query;
      Found  : Boolean;
   begin
      Log.Info ("Authenticate user {0}", Email);

      Ctx.Start;

      --  Find the user registered under the given email address & password.
      Query.Bind_Param (1, Email);
      Query.Bind_Param (2, Password);
      Query.Set_Join ("inner join Email e on e.user_id = o.id");
      Query.Set_Filter ("e.email = ? and o.password = ?");
      User.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No user registered under email address {0} or invalid password",
                   Email);
         raise Not_Found with "No user registered under email: " & Email;
      end if;

      --  Create the session
      Session := Session_Ref'Class (Null_Session);
      Session.Set_Start_Date (ADO.Nullable_Time '(Value => Ada.Calendar.Clock, Is_Null => False));
      Session.Set_User_Id (User.Get_Id);
      Session.Set_Ip_Address (IpAddr);
      Session.Save (DB);

      Ctx.Commit;
	  Log.Info ("Session {0} created for user {1}",
                ADO.Identifier'Image (Session.Get_Id), Email);
   end Authenticate;

   --  ------------------------------
   --  Start the lost password process for a user.  Find the user having
   --  the given email address and send that user a password reset key
   --  in an email.
   --  Raises Not_Found exception if no user with such email exist
   --  ------------------------------
   procedure Lost_Password (Model : in User_Service;
                            Email : in String) is
      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      User   : User_Ref;
      Key    : Access_Key_Ref;
      Query  : ADO.SQL.Query;
      Found  : Boolean;
   begin
      Log.Info ("Lost password for {0}", Email);

      Ctx.Start;

      --  Find the user with the given email address.
      Query.Set_Join ("inner join Email e on e.user_id = o.id");
      Query.Set_Filter ("e.email = ?");
      Query.Bind_Param (1, Email);
      User.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No user with email address {0}", Email);
         raise Not_Found with "No user registered under email: " & Email;
      end if;

      --  Create the secure key to change the password
      Key.Set_Access_Key (Create_Key (User.Get_Id));
      Key.Set_User_Id (User.Get_Id);
      Key.Save (DB);

      --  Send the email with the reset password key
      declare
         Event : ASF.Events.Modules.Module_Event;
      begin
         Event.Set_Parameter ("key", Key.Get_Access_Key);
         Event.Set_Parameter ("email", Email);
         Model.Send_Alert ("lost-password", User, Event);
      end;

      Ctx.Commit;
   end Lost_Password;

   --  ------------------------------
   --  Reset the password of the user associated with the secure key.
   --  to the user in an email.
   --  Raises Not_Found if there is no key or if the user does not have any email
   --  ------------------------------
   procedure Reset_Password (Model    : in User_Service;
                             Key      : in String;
                             Password : in String;
                             IpAddr   : in String;
                             User     : out User_Ref'Class;
                             Session  : out Session_Ref'Class) is
      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query  : ADO.SQL.Query;
      Found  : Boolean;
      Email  : Email_Ref;
      Access_Key : Access_Key_Ref;
   begin
      Log.Info ("Reset password with key {0}", Key);

      Ctx.Start;

      --  Find the user associated with the key.
      Query.Bind_Param (1, Key);
      Query.Set_Filter ("access_key = ?");
      Access_Key.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("Invalid reset password key {0}", Key);
         raise Not_Found with "No such access key: " & Key;
      end if;

      User.Load (DB, Access_Key.Get_User_Id, Found);
      if not Found then
         Log.Warn ("No user associated with access key {0}", Key);
         raise Not_Found with "No user associated with access key: " & Key;
      end if;

      --  Get the user primary email address.
      Email.Load (DB, User.Get_Email.Get_Id, Found);
      if not Found then
         Log.Warn ("No email address associated with user {0}", User.Get_Name);
         raise Not_Found with "No email address for user";
      end if;

      --  Delete the access key.
      Access_Key.Delete (DB);

      --  Reset the user password
      User.Set_Password (Password);
      User.Save (DB);

      --  Create the authentication session
      Session := Session_Ref'Class (Null_Session);
      Session.Set_User_Id (User.Get_Id);
      Session.Set_Start_Date (ADO.Nullable_Time '(Value => Ada.Calendar.Clock, Is_Null => False));
      Session.Set_Ip_Address (IpAddr);
      Session.Save (DB);

      --  Send the email to warn about the password change
      declare
         Event : ASF.Events.Modules.Module_Event;
      begin
         Event.Set_Parameter ("ip_address", IpAddr);
         Event.Set_Parameter ("email", Email.Get_Email);
         Model.Send_Alert ("reset-password", User, Event);
      end;

      Ctx.Commit;
   end Reset_Password;

   --  ------------------------------
   --  Create a user in the database with the given user information and
   --  the associated email address.  Verify that no such user already exist.
   --  Raises User_Exist exception if a user with such email is already registered.
   --  ------------------------------
   procedure Create_User (Model : in User_Service;
                          User  : in out User_Ref'Class;
                          Email : in out Email_Ref'Class) is
      COUNT_SQL : constant String := "select count(*) from Email where email = ?";

      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Key    : Access_Key_Ref;
      Stmt   : Query_Statement := DB.Create_Statement (COUNT_SQL);
      Email_Address : constant String := Email.Get_Email;
   begin
      Log.Info ("Create user {0}", Email_Address);

      Ctx.Start;

      --  Check first if this user is already known
      Stmt.Bind_Param (1, Email_Address);
      Stmt.Execute;
      if Stmt.Has_Elements and then Stmt.Get_Integer (0) > 0 then
         Log.Warn ("User {0} already registered", Email_Address);
         raise User_Exist with "Email address " & Email_Address & "' already used";
      end if;

      --  Save the email and the user
      Email.Set_User_Id (0);
      Email.Save (DB);
      User.Set_Email (Email);
      User.Save (DB);

      Email.Set_User_Id (User.Get_Id);
      Email.Save (DB);

      --  Create the access key
      Key.Set_Access_Key (Create_Key (Email.Get_Id));
      Key.Set_User_Id (User.Get_Id);
      Key.Save (DB);

      --  Send the email with the access key to finish the user creation
      declare
         Event : ASF.Events.Modules.Module_Event;
      begin
         Event.Set_Parameter ("key", Key.Get_Access_Key);
         Event.Set_Parameter ("email", Email_Address);
         Model.Send_Alert ("create-user", User, Event);
      end;

      Ctx.Commit;
   end Create_User;

   --  ------------------------------
   --  Verify the access key and retrieve the user associated with that key.
   --  Starts a new session associated with the given IP address.
   --  Raises Not_Found if the access key does not exist.
   --  ------------------------------
   procedure Verify_User (Model    : in User_Service;
                          Key      : in String;
                          IpAddr   : in String;
                          User     : out User_Ref'Class;
                          Session  : out Session_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query  : ADO.SQL.Query;
      Found  : Boolean;
      Access_Key : Access_Key_Ref;
   begin
      Log.Info ("Verify user with key {0}", Key);

      Ctx.Start;

      --  Find the user associated with the given key
      Query.Bind_Param (1, Key);
      Query.Set_Filter ("access_key = ?");
      Access_Key.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No access key {0}", Key);
         raise Not_Found with "No access key: " & Key;
      end if;

      User.Load (DB, Access_Key.Get_User_Id, Found);
      if not Found then
         Log.Warn ("No user linked to access key {0}", Key);
         raise Not_Found with "No user for access key: " & Key;
      end if;

      Access_Key.Delete (DB);

      --  Create the session
      Session := Session_Ref'Class (Null_Session);
      Session.Set_Start_Date (ADO.Nullable_Time '(Value => Ada.Calendar.Clock, Is_Null => False));
      Session.Set_User_Id (User.Get_Id);
      Session.Set_Ip_Address (IpAddr);
      Session.Save (DB);

      Ctx.Commit;
   end Verify_User;

   --  ------------------------------
   --  Verify that the user session identified by <b>Id</b> is valid and still active.
   --  Returns the user and the session objects.
   --  Raises Not_Found if the session does not exist or was closed.
   --  ------------------------------
   procedure Verify_Session (Model   : in User_Service;
                             Id      : in ADO.Identifier;
                             User    : out User_Ref'Class;
                             Session : out Session_Ref'Class) is
      pragma Unreferenced (Model);

      Sid    : constant String := ADO.Identifier'Image (Id);
      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : ADO.Sessions.Session := AWA.Services.Contexts.Get_Session (Ctx);
      Query  : ADO.SQL.Query;
      Found  : Boolean;
   begin
      Log.Info ("Verify user session {0}", Sid);

      Query.Set_Filter ("id = ? and end_date is null");
      Query.Bind_Param (1, Id);
      Session.Find (Session => DB,
                    Query   => Query,
                    Found   => Found);
      if not Found then
         Log.Warn ("Session {0} is not found or closed", Sid);
         raise Not_Found with "Session not found: " & Sid;
      end if;

      User.Load (Session => DB, Id => Session.Get_User_Id, Found => Found);
      if not Found then
         Log.Error ("User linked to session {0} does not exist", Sid);
         raise Not_Found with "Session not found: " & Sid;
      end if;
   end Verify_Session;

   --  ------------------------------
   --  Closes the session identified by <b>Id</b>.
   --  ------------------------------
   procedure Close_Session (Model : in User_Service;
                            Id    : in ADO.Identifier) is
      pragma Unreferenced (Model);

      Sid     : constant String := ADO.Identifier'Image (Id);
      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Session : Session_Ref;
      Found   : Boolean;
   begin
      Log.Info ("Closing user session {0}", Sid);

      Ctx.Start;

      Session.Load (DB, Id, Found);
      if not Found then
         Log.Warn ("Session {0} is not found", Sid);
         raise Not_Found with "Session not found: " & Sid;
      end if;

      --  The end date must be null.  Otherwise, it means the session was closed already.
      if not Session.Get_End_Date.Is_Null then
         Log.Warn ("Session {0} is already closed", Sid);
         raise Not_Found with "Session is closed: " & Sid;
      end if;
      Session.Set_End_Date (ADO.Nullable_Time '(Value => Ada.Calendar.Clock, Is_Null => False));
      Session.Save (DB);
      Ctx.Commit;
   end Close_Session;

end AWA.Users.Services;
