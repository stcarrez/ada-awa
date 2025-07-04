-----------------------------------------------------------------------
--  awa-users-services -- User registration, authentication processes
--  Copyright (C) 2009 - 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings;
with Ada.Calendar;

with Util.Log.Loggers;
with Util.Strings.Transforms;
with Util.Mail;
with Util.Encoders.HMAC.SHA1;

with ADO.SQL;
with ADO.Statements;
with ADO.Objects;

with Security.Contexts;
with Security.Policies;

with AWA.Services;
with AWA.Services.Contexts;
package body AWA.Users.Services is

   use Util.Log;
   use ADO.Statements;
   use ADO.Sessions;
   use Ada.Strings.Unbounded;
   use ADO.SQL;
   use AWA.Services;
   use type ADO.Identifier;

   package ASC renames AWA.Services.Contexts;

   Log : constant Loggers.Logger := Loggers.Create ("AWA.Users.Services");

   procedure Send_Alert (Model : in User_Service;
                         Kind  : in AWA.Events.Event_Index;
                         User  : in User_Ref'Class;
                         Props : in out AWA.Events.Module_Event) is
   begin
      Props.Set_Event_Kind (Kind);
      Props.Set_Parameter ("first_name", User.Get_First_Name);
      Props.Set_Parameter ("last_name", User.Get_Last_Name);
      Props.Set_Parameter ("name", User.Get_Name);
      Model.Send_Event (Props);
   end Send_Alert;

   function Create_Key (Model  : in out User_Service;
                        Number : in ADO.Identifier) return String is
      Rand : constant String := Model.Random.Generate (256);
   begin
      Log.Info ("Random {0}", Rand);
      return Util.Encoders.HMAC.SHA1.Sign_Base64 (Key  => Rand,
                                                  Data => ADO.Identifier'Image (Number),
                                                  URL  => True);
   end Create_Key;

   --  ------------------------------
   --  Build the authenticate cookie.  The cookie is signed using HMAC-SHA1 with a private key.
   --  ------------------------------
   function Get_Authenticate_Cookie (Model : in User_Service;
                                     Id    : in ADO.Identifier)
                                     return String is
      Ident : constant String := Util.Strings.Image (Integer (Id));
      Key   : constant String := To_String (Model.Auth_Key);
   begin
      return Ident & '.' & Util.Encoders.HMAC.SHA1.Sign_Base64 (Key  => Key, Data => Ident);
   end Get_Authenticate_Cookie;

   --  ------------------------------
   --  Get the password hash.  The password is signed using HMAC-SHA1 with the salt.
   --  ------------------------------
   function Get_Password_Hash (Model    : in User_Service;
                               Password : in String;
                               Salt     : in String)
                               return String is
      pragma Unreferenced (Model);
   begin
      return Util.Encoders.HMAC.SHA1.Sign_Base64 (Key => Salt, Data => Password);
   end Get_Password_Hash;

   --  ------------------------------
   --  Get the user name from the email address.
   --  Returns the possible user name from his email address.
   --  ------------------------------
   function Get_Name_From_Email (Email : in String) return String is
      E    : constant Util.Mail.Email_Address := Util.Mail.Parse_Address (Email);
      Name : String := To_String (E.Name);
   begin
      for I in Name'Range loop
         if Name (I) = '.' then
            Name (I) := ' ';
         end if;
      end loop;
      return Util.Strings.Transforms.Capitalize (Name);
   end Get_Name_From_Email;

   --  ------------------------------
   --  Get the authenticate identifier from the cookie.
   --  Verify that the cookie is valid, the signature is correct.
   --  Returns the identified or NO_IDENTIFIER
   --  ------------------------------
   function Get_Authenticate_Id (Model  : in User_Service;
                                 Cookie : in String) return ADO.Identifier is
      Pos : constant Natural := Util.Strings.Index (Cookie, '.');
      Id  : ADO.Identifier;
   begin
      if Pos <= 1 then
         return ADO.NO_IDENTIFIER;
      end if;

      Id := ADO.Identifier'Value (Cookie (Cookie'First .. Pos - 1));
      if Cookie /= Model.Get_Authenticate_Cookie (Id) then
         return ADO.NO_IDENTIFIER;
      end if;

      return Id;

   exception
      when others =>
         return ADO.NO_IDENTIFIER;
   end Get_Authenticate_Id;

   procedure Create_Session (Model   : in User_Service;
                             DB      : in out Master_Session;
                             Session : out Session_Ref'Class;
                             User    : in User_Ref'Class;
                             Auth    : in Authenticate_Ref'Class;
                             Ip_Addr : in String;
                             Principal : out AWA.Users.Principals.Principal_Access) is
      Ctx          : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      Auth_Session : Session_Ref;
      Dummy_Email  : constant String := User.Get_Email.Get_Email;
   begin
      --  Create the authenticate session.
      Auth_Session.Set_Start_Date (Ada.Calendar.Clock);
      Auth_Session.Set_User (User);
      Auth_Session.Set_Ip_Address (Ip_Addr);
      Auth_Session.Set_Stype (Users.Models.AUTH_SESSION);
      Auth_Session.Set_Server_Id (Model.Server_Id);
      Auth_Session.Set_User_Auth (Auth);
      Auth_Session.Save (DB);

      --  Create the connection session.
      Session := Session_Ref'Class (Null_Session);
      Session.Set_Start_Date (Auth_Session.Get_Start_Date);
      Session.Set_User (User);
      Session.Set_Ip_Address (Ip_Addr);
      Session.Set_Stype (Users.Models.CONNECT_SESSION);
      Session.Set_Auth (Auth_Session);
      Session.Set_User_Auth (Auth);
      Session.Set_Server_Id (Model.Server_Id);
      Session.Save (DB);

      Principal := AWA.Users.Principals.Create (User_Ref (User), Session_Ref (Session));
      Ctx.Set_Context (Ctx.Get_Application, Principal);
   end Create_Session;

   --  ------------------------------
   --  Authenticate the user with his OpenID identifier.  The authentication process
   --  was made by an external OpenID provider.  If the user does not yet exists in
   --  the database, a record is created for him.  Create a new session for the user.
   --  The IP address of the connection is saved in the session.
   --  Raises Not_Found exception if the user is not recognized
   --  ------------------------------
   procedure Authenticate (Model     : in User_Service;
                           Auth      : in Security.Auth.Authentication;
                           IpAddr    : in String;
                           Principal : out AWA.Users.Principals.Principal_Access) is
      --  Update the user first name/last name
      procedure Update_User;

      OpenId        : constant String := Security.Auth.Get_Claimed_Id (Auth);
      Email_Address : constant String := Security.Auth.Get_Email (Auth);
      Ctx           : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB            : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query         : ADO.SQL.Query;
      Found_Email   : Boolean;
      Found_Auth    : Boolean;
      Found_User    : Boolean;
      User          : User_Ref;
      Email         : Email_Ref;
      User_Auth     : Authenticate_Ref;
      Session       : Session_Ref;

      --  ------------------------------
      --  Update the user first name/last name
      --  ------------------------------
      procedure Update_User is
         Name       : constant String := Security.Auth.Get_Full_Name (Auth);
         First_Name : constant String := Security.Auth.Get_First_Name (Auth);
         Last_Name  : constant String := Security.Auth.Get_Last_Name (Auth);
         Sep        : constant Natural := Util.Strings.Index (Name, ' ');
      begin
         if Name'Length > 0 and then Name /= String '(User.Get_Name) then
            User.Set_Name (Name);
         end if;
         if First_Name'Length > 0 and then First_Name /= String '(User.Get_First_Name) then
            User.Set_First_Name (First_Name);
         end if;
         if Last_Name'Length > 0 and then Last_Name /= String '(User.Get_Last_Name) then
            User.Set_Last_Name (Last_Name);
         end if;
         if Sep > 0 and then String '(User.Get_First_Name) = "" then
            User.Set_First_Name (Name (Name'First .. Sep - 1));
         end if;
         if Sep > 0 and then String '(User.Get_Last_Name) = "" then
            User.Set_Last_Name (Name (Sep + 1 .. Name'Last));
         end if;
         if Name'Length > 0 and then String '(User.Get_First_Name) = "" then
            User.Set_First_Name (Name);
         end if;
         if Name'Length = 0 then
            User.Set_Name (Get_Name_From_Email (Email => Email_Address));
         end if;
         User.Save (DB);
      end Update_User;

   begin
      Log.Info ("Authenticated user {0}", Email_Address);

      Ctx.Start;

      --  Find the email address.
      Query.Set_Filter ("LOWER(o.email) = LOWER(:email)");
      Query.Bind_Param ("email", Email_Address);
      Email.Find (DB, Query, Found_Email);

      --  Find the authenticate information for the given OpenID identifier.
      Query.Clear;
      Query.Set_Filter ("o.ident = :ident AND o.method = :method");
      Query.Bind_Param ("ident", OpenId);
      Query.Bind_Param ("method", Natural (1));
      User_Auth.Find (DB, Query, Found_Auth);

      if Found_Auth then
         User := User_Ref (User_Auth.Get_User);
         Found_User := True;
      elsif Found_Email then
         if Email.Get_User_Id > 0 then
            User.Load (DB, Email.Get_User_Id, Found_User);
         else
            Found_User := False;
         end if;
         User_Auth.Set_Email (Email);
      else
         Found_User := False;
      end if;

      --  User is not found, registration must be enabled to create it.
      if not Found_User and then not Model.Allow_Register then
         Log.Warn ("Registration disabled: no user associated with email {0}",
                   Email_Address);
         raise Registration_Disabled;
      end if;

      --  Email is not known and we have a new user: create it.
      if not Found_Email and not Found_User then
         Log.Info ("Creating email record for {0}", Email_Address);
         Email.Set_Email (Email_Address);
         Email.Save (DB);
      end if;

      if not Found_User then
         Log.Info ("User {0} is not known", Email_Address);

         User.Set_Email (Email);
         User.Set_Status (Models.USER_ENABLED);
         Update_User;
         Email.Set_User_Id (User.Get_Id);
         Email.Save (DB);
         User_Lifecycle.Notify_Create (Model, User);
      elsif User.Get_Status = Models.USER_DISABLED then
         raise User_Disabled;
      else
         if User.Get_Status = Models.USER_REGISTERED then
            User.Set_Status (Models.USER_ENABLED);
         end if;
         Update_User;

         --  The user email address could have changed
         declare
            E : Email_Ref'Class := User.Get_Email;
         begin
            if Email_Address /= String '(E.Get_Email) then
               Log.Info ("Changing email address from {0} to {1} for user {2}",
                         String '(E.Get_Email), Email_Address, OpenId);
               E.Set_Email (Email_Address);
               E.Save (DB);
            end if;
         end;
         User_Lifecycle.Notify_Update (Model, User);
      end if;

      if not Found_Auth then
         User_Auth.Set_Email (Email);
         User_Auth.Set_User (User);
         User_Auth.Set_Ident (OpenId);
         User_Auth.Set_Method (AUTH_OAUTH);
         User_Auth.Save (DB);
      end if;

      Create_Session (Model, DB, Session, User, User_Auth, IpAddr, Principal);
      if not Found_User then
         declare
            Event   : AWA.Events.Module_Event;
            Sec_Ctx : Security.Contexts.Security_Context;
         begin
            --  Make a security context with the user's credential.
            Sec_Ctx.Set_Context (Manager   => Model.Permissions.all'Access,
                                 Principal => Principal.all'Access);

            --  Send the event to indicate a new user was created.
            Event.Set_Parameter ("email", Email_Address);
            Model.Send_Alert (User_Create_Event.Kind, User, Event);
         end;
      end if;
      Ctx.Commit;
   end Authenticate;

   --  ------------------------------
   --  Authenticate the user with his email address and his password.
   --  If the user is authenticated, return the user information and
   --  create a new session.  The IP address of the connection is saved
   --  in the session.
   --  Raises Not_Found exception if the user is not recognized
   --  ------------------------------
   procedure Authenticate (Model     : in User_Service;
                           Email     : in String;
                           Password  : in String;
                           IpAddr    : in String;
                           Principal : out AWA.Users.Principals.Principal_Access) is

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB      : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query   : ADO.SQL.Query;
      Found   : Boolean;
      User    : User_Ref;
      Session : Session_Ref;
      Auth    : Authenticate_Ref;
   begin
      Log.Info ("Authenticate user {0}", Email);

      --  Find the user registered under the given email address & password.
      Query.Bind_Param (1, Email);
      Query.Set_Join ("INNER JOIN awa_email e ON e.user_id = o.id");
      Query.Set_Filter ("LOWER(e.email) = LOWER(?)");
      User.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No user registered under email address {0} or invalid password",
                   Email);
         raise Not_Found with "No user registered under email: " & Email;
      end if;

      --  Reject authentication on disabled the user account.
      if User.Get_Status /= Models.USER_ENABLED then
         Log.Warn ("User account {0} is disabled", Email);
         raise User_Disabled with "User account is disabled";
      end if;

      Query.Clear;
      Query.Set_Filter ("o.email_id = ? AND o.method = 0");
      Query.Bind_Param (1, User.Get_Email.Get_Id);
      Auth.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No password defined for user {0}", Email);
         raise Not_Found with "No password defined with email: " & Email;
      end if;
      declare
         Salt : constant String := Auth.Get_Salt;
         Hash : constant String
            := User_Service'Class (Model).Get_Password_Hash (Salt, Password);
      begin
         --  Reject authentication if the salt is empty: this account is not
         --  validated yet for password authentication.
         if Salt'Length = 0 then
            Log.Warn ("Empty password salt for user {0}", Email);
            raise User_Disabled with "User account is not validated: " & Email;
         end if;

         if Hash /= String '(Auth.Get_Hash) then
            Log.Warn ("Invalid password for user {0}", Email);
            raise Not_Found with "No user registered under email: " & Email;
         end if;
      end;

      Ctx.Start;

      Create_Session (Model, DB, Session, User, Auth, IpAddr, Principal);

      Ctx.Commit;
      Log.Info ("Session {0} created for user {1}",
                ADO.Identifier'Image (Session.Get_Id), Email);
   end Authenticate;

   --  ------------------------------
   --  Authenticate the user with the authenticate cookie generated from a previous authenticate
   --  session.  If the cookie has the correct signature, matches a valid session,
   --  return the user information and create a new session.  The IP address of the connection
   --  is saved in the session.
   --  Raises Not_Found exception if the user is not recognized
   --  ------------------------------
   procedure Authenticate (Model    : in User_Service;
                           Cookie   : in String;
                           Ip_Addr  : in String;
                           Principal : out AWA.Users.Principals.Principal_Access) is
      use type Contexts.Service_Context_Access;

      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : Master_Session;
      Found  : Boolean;

      Cookie_Session : Session_Ref;
      Auth_Session   : Session_Ref;
      Session        : Session_Ref;
      User           : User_Ref;
      Query          : ADO.SQL.Query;
      Id             : constant ADO.Identifier := Model.Get_Authenticate_Id (Cookie);
   begin
      Log.Info ("Authenticate cookie {0}", Cookie);

      if Id = ADO.NO_IDENTIFIER then
         Log.Warn ("Invalid authenticate cookie: {0}", Cookie);
         raise Not_Found with "Invalid cookie";
      end if;
      DB := Model.Get_Master_Session;

      --  Find the user registered under the given email address & password.
      Query.Bind_Param (1, Id);
      Query.Set_Join ("INNER JOIN awa_user AS u ON u.id = o.user_id");
      Query.Set_Filter ("o.id = ? AND o.end_date IS NULL");
      Cookie_Session.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("Authenticate session {0} not found in database", ADO.Identifier'Image (Id));
         raise Not_Found with "Invalid cookie";
      end if;

      --  Reject authentication on disabled the user account.
      if User.Get_Status /= Models.USER_ENABLED then
         Log.Warn ("Authenticate session {0} is disabled", ADO.Identifier'Image (Id));
         raise User_Disabled with "User account is disabled";
      end if;

      if Cookie_Session.Get_Stype /= Users.Models.CONNECT_SESSION then
         Log.Warn ("Authenticate session {0} not found in database", ADO.Identifier'Image (Id));
         raise Not_Found with "Invalid cookie";
      end if;

      Auth_Session := Session_Ref (Cookie_Session.Get_Auth);
      if not Auth_Session.Get_End_Date.Is_Null then
         Log.Warn ("Authenticate session was closed");
         raise Not_Found with "Authenticate session was closed.";
      end if;

      User := User_Ref (Auth_Session.Get_User);

      Session.Set_Start_Date (Ada.Calendar.Clock);
      Session.Set_User (User);
      Session.Set_Ip_Address (Ip_Addr);
      Session.Set_Stype (Users.Models.CONNECT_SESSION);
      Session.Set_Auth (Auth_Session);
      Session.Set_Server_Id (Model.Server_Id);
      Session.Save (DB);

      Principal := AWA.Users.Principals.Create (User, Session);
      if Ctx /= null then
         Ctx.Set_Context (Ctx.Get_Application, Principal);
      end if;

      --  Mark the cookie session as used.
      Cookie_Session.Set_Stype (Users.Models.USED_SESSION);
      if Cookie_Session.Get_End_Date.Is_Null then
         Cookie_Session.Set_End_Date (ADO.Nullable_Time '(Value => Session.Get_Start_Date,
                                                          Is_Null => False));
      end if;
      Cookie_Session.Save (DB);

      Log.Info ("Session {0} created for user {1}",
                ADO.Identifier'Image (Session.Get_Id), ADO.Identifier'Image (User.Get_Id));

   exception
      when ADO.Objects.NOT_FOUND =>
         Log.Warn ("No user associated with session {0}", ADO.Identifier'Image (Id));
         raise Not_Found with "Invalid cookie";

   end Authenticate;

   --  ------------------------------
   --  Create and generate a new access key for the user.  The access key is saved in the
   --  database and it will expire after the expiration delay.
   --  ------------------------------
   procedure Create_Access_Key (Model   : in out User_Service;
                                User    : in AWA.Users.Models.User_Ref'Class;
                                Key     : in out AWA.Users.Models.Access_Key_Ref;
                                Kind    : in AWA.Users.Models.Key_Type;
                                Expire  : in Duration;
                                Session : in out ADO.Sessions.Master_Session) is
      use type Ada.Calendar.Time;
   begin
      Key.Set_Access_Key (Model.Create_Key (User.Get_Id));
      Key.Set_Expire_Date (Ada.Calendar.Clock + Expire);
      Key.Set_Kind (Kind);
      Key.Set_User (User);
      Key.Save (Session);
   end Create_Access_Key;

   --  ------------------------------
   --  Start the lost password process for a user.  Find the user having
   --  the given email address and send that user a password reset key
   --  in an email.
   --  Raises Not_Found exception if no user with such email exist
   --  ------------------------------
   procedure Lost_Password (Model : in out User_Service;
                            Email : in String) is
      Ctx    : constant ASC.Service_Context_Access := ASC.Current;
      DB     : Master_Session := ASC.Get_Master_Session (Ctx);
      User   : User_Ref;
      Key    : Access_Key_Ref;
      Query  : ADO.SQL.Query;
      Found  : Boolean;
      Stmt   : ADO.Statements.Delete_Statement;
   begin
      Log.Info ("Lost password for {0}", Email);

      Ctx.Start;

      --  Find the user with the given email address.
      Query.Set_Join ("INNER JOIN awa_email e ON e.user_id = o.id");
      Query.Set_Filter ("LOWER(e.email) = LOWER(?)");
      Query.Bind_Param (1, Email);
      User.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No user with email address {0}", Email);
         raise Not_Found with "No user registered under email: " & Email;
      end if;

      --  Reject lost password disabled the user account.
      if User.Get_Status = Models.USER_DISABLED then
         Log.Warn ("User account {0} is disabled", Email);
         raise User_Disabled with "User account is disabled";
      end if;

      --  Delete any previous reset password access key for the user.
      Stmt := DB.Create_Statement (AWA.Users.Models.ACCESS_KEY_TABLE);
      Stmt.Set_Filter ("user_id = ? and kind = ?");
      Stmt.Bind_Param (1, User.Get_Id);
      Stmt.Bind_Param (2, Integer (Models.Key_Type'Pos (Models.RESET_PASSWORD_KEY)));
      Stmt.Execute;

      --  Create the secure key to change the password
      Model.Create_Access_Key (User    => User,
                               Key     => Key,
                               Kind    => AWA.Users.Models.RESET_PASSWORD_KEY,
                               Expire  => 86400.0,
                               Session => DB);

      --  Send the email with the reset password key
      declare
         Event : AWA.Events.Module_Event;
      begin
         Event.Set_Parameter ("key", Key.Get_Access_Key);
         Event.Set_Parameter ("email", Email);
         Model.Send_Alert (User_Lost_Password_Event.Kind, User, Event);
      end;

      Ctx.Commit;
   end Lost_Password;

   --  ------------------------------
   --  Reset the password of the user associated with the secure key.
   --  Raises Not_Found if there is no key or if the user does not have any email
   --  ------------------------------
   procedure Reset_Password (Model    : in out User_Service;
                             Key      : in String;
                             Password : in String;
                             IpAddr   : in String;
                             Principal : out AWA.Users.Principals.Principal_Access) is
      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB     : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Query  : ADO.SQL.Query;
      Found  : Boolean;
      Email  : Email_Ref;
      Access_Key : Access_Key_Ref;
      User       : User_Ref;
      Auth       : Authenticate_Ref;
      Session    : Session_Ref;
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

      User := User_Ref (Access_Key.Get_User);

      --  Delete the access key.
      Access_Key.Delete (DB);

      --  Reject reset password disabled the user account.
      if User.Get_Status = Models.USER_DISABLED then
         Ctx.Commit;
         Log.Warn ("User account {0} is disabled", Email.Get_Email);
         raise User_Disabled with "User account is disabled";
      end if;

      --  Get the user primary email address.
      Email.Load (DB, User.Get_Email.Get_Id, Found);
      if not Found then
         Ctx.Commit;
         Log.Warn ("User email address not found {0}", Email.Get_Email);
         raise Not_Found with "User email address not found: " & Email.Get_Email;
      end if;

      --  Get the authenticate information for the email address
      --  (keep only AUTH_HASH_SHA1).
      Query.Clear;
      Query.Set_Filter ("o.method = 0 AND o.email_id = :email_id");
      Query.Bind_Param ("email_id", Email.Get_Id);
      Auth.Find (DB, Query, Found);
      if not Found then
         Auth.Set_User (User);
         Auth.Set_Email (Email);
         Auth.Set_Method (AUTH_HASH_SHA1);
         Auth.Set_Ident (String '(Email.Get_Email));
      end if;

      --  Reset the user password
      Auth.Set_Salt (Model.Create_Key (User.Get_Id));
      Auth.Set_Hash (User_Service'Class (Model).Get_Password_Hash (Auth.Get_Salt, Password));
      Auth.Save (DB);

      User.Set_Status (Models.USER_ENABLED);
      User.Save (DB);

      --  Create the authentication session.
      Create_Session (Model, DB, Session, User, Auth, IpAddr, Principal);

      --  Send the email to warn about the password change
      declare
         Event : AWA.Events.Module_Event;
      begin
         Event.Set_Parameter ("ip_address", IpAddr);
         Event.Set_Parameter ("email", Email.Get_Email);
         Model.Send_Alert (User_Reset_Password_Event.Kind, User, Event);
      end;

      Ctx.Commit;

   exception
      when ADO.Objects.NOT_FOUND =>
         Log.Warn ("No user associated with access key {0}", Key);
         raise Not_Found with "No user associated with access key: " & Key;

   end Reset_Password;

   --  ------------------------------
   --  Create a user in the database with the given user information and
   --  the associated email address.  Verify that no such user already exist.
   --  Build an access key that allows to verify the user email and finish
   --  the account creation.
   --  Raises User_Exist exception if a user with such email is already registered.
   --  ------------------------------
   procedure Create_User (Model    : in out User_Service;
                          User     : in out User_Ref'Class;
                          Email    : in out Email_Ref'Class;
                          Password : in String;
                          Key      : in out Access_Key_Ref'Class;
                          Send     : in Boolean) is
      COUNT_SQL : constant String
        := "SELECT COUNT(*) FROM awa_email WHERE LOWER(email) = LOWER(?)";

      Ctx           : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB            : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Stmt          : Query_Statement := DB.Create_Statement (COUNT_SQL);
      Email_Address : constant String := Email.Get_Email;
   begin
      Log.Info ("Create user {0}", Email_Address);

      --  Reject user creation if the registration is disabled.
      if not Model.Allow_Register then
         Log.Warn ("Registration disabled: cannot register user with email {0}",
                   Email_Address);
         raise Registration_Disabled;
      end if;

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
      User.Set_Status (Models.USER_REGISTERED);
      if String '(User.Get_Name) = "" then
         User.Set_Name (String '(User.Get_First_Name) & " " & String '(User.Get_Last_Name));
      end if;
      User.Save (DB);

      Email.Set_User_Id (User.Get_Id);
      Email.Save (DB);

      --  Make a random salt and generate the password hash.
      --  If there is no password, keep an empty salt/password,
      --  this will be refused at authentication.
      if Password'Length > 0 then
         declare
            Auth : Authenticate_Ref;
         begin
            Auth.Set_User (User);
            Auth.Set_Salt (Model.Create_Key (User.Get_Id));
            Auth.Set_Hash
              (User_Service'Class (Model).Get_Password_Hash (Auth.Get_Salt, Password));
            Auth.Set_Email (Email);
            Auth.Set_Ident (Email_Address);
            Auth.Set_Method (AUTH_HASH_SHA1);
            Auth.Save (DB);
         end;
      end if;

      --  Create the access key
      Model.Create_Access_Key (User    => User,
                               Key     => Access_Key_Ref (Key),
                               Kind    => AWA.Users.Models.SIGNUP_KEY,
                               Expire  => 86400.0,
                               Session => DB);

      --  Send the email with the access key to finish the user registration.
      if Send then
         declare
            Event : AWA.Events.Module_Event;
         begin
            Event.Set_Parameter ("key", Key.Get_Access_Key);
            Event.Set_Parameter ("email", Email_Address);
            Model.Send_Alert (User_Register_Event.Kind, User, Event);
         end;
      end if;

      Ctx.Commit;
   end Create_User;

   --  ------------------------------
   --  Create a user in the database with the given user information and
   --  the associated email address and for the given access key.  The access key is first
   --  verified and the user instance associated with it is retrieved.  Verify that the email
   --  address is unique and can be used by the user.  Since the access key is verified,
   --  grant immediately the access by opening a session and setting up the principal instance.
   --  Raises User_Exist exception if a user with such email is already registered.
   --  ------------------------------
   procedure Create_User (Model     : in out User_Service;
                          User      : in out User_Ref'Class;
                          Email     : in out Email_Ref'Class;
                          Password  : in String;
                          Key       : in String;
                          IpAddr    : in String;
                          Principal : out AWA.Users.Principals.Principal_Access) is

      Ctx           : constant ASC.Service_Context_Access := ASC.Current;
      DB            : Master_Session := ASC.Get_Master_Session (Ctx);
      Access_Key    : Access_Key_Ref;
      Query         : ADO.SQL.Query;
      Session       : Session_Ref;
      Exist_Email   : Email_Ref;
      Email_Address : constant String := Email.Get_Email;
      Found         : Boolean;
      Cur_User      : User_Ref;
      Cur_Email     : Email_Ref;
      Auth          : Authenticate_Ref;
   begin
      Log.Info ("Create user {0} with key {1}", Email_Address, Key);

      --  We want to allow the user creation if the access key is enabled
      --  because that access key was sent by the site administrator and
      --  we trust it.  If the access key is invalid, the user creation is rejected.
      if not Model.Allow_Register then
         Log.Info ("Registration is disabled but an access key is used with email {0}",
                   Email_Address);
      end if;

      Ctx.Start;

      --  Verify the access key validity.
      Query.Bind_Param (1, Key);
      Query.Set_Filter ("access_key = ?");
      Access_Key.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No access key {0}", Key);
         raise Not_Found with "No access key: " & Key;
      end if;
      Cur_User := User_Ref (Access_Key.Get_User);
      Cur_Email := Email_Ref (Cur_User.Get_Email);

      --  Check first if the email address is not used by another user.
      Query.Bind_Param (1, Email_Address);
      Query.Set_Filter ("LOWER(email) = LOWER(?)");
      Exist_Email.Find (DB, Query, Found);
      if Found and then Exist_Email.Get_Id /= Cur_Email.Get_Id then
         Ctx.Commit;
         Log.Warn ("Email address {0} already registered", Email_Address);
         raise User_Exist with "Email address " & Email_Address & "' already used";
      end if;

      --  Make sure the user is not disabled.
      if Cur_User.Get_Status = Models.USER_DISABLED then
         Access_Key.Delete (DB);
         Ctx.Commit;
         Log.Warn ("User account {0} is disabled", Email_Address);
         raise User_Disabled with "User account '" & Email_Address & "' is disabled";
      end if;

      --  Save the email and the user
      Cur_Email.Set_User_Id (Cur_User.Get_Id);
      Cur_Email.Set_Email (Email_Address);
      Cur_Email.Save (DB);

      Cur_User.Set_Name (String '(User.Get_Name));
      Cur_User.Set_First_Name (String '(User.Get_First_Name));
      Cur_User.Set_Last_Name (String '(User.Get_Last_Name));
      if String '(User.Get_Name) = "" then
         User.Set_Name (String '(User.Get_First_Name) & " " & String '(User.Get_Last_Name));
      end if;

      --  Get the authenticate information for the email address
      --  (keep only AUTH_HASH_SHA1).
      Query.Clear;
      Query.Set_Filter ("o.method = 0 AND o.email_id = :email_id");
      Query.Bind_Param ("email_id", Cur_Email.Get_Id);
      Auth.Find (DB, Query, Found);
      if not Found then
         Auth.Set_User (Cur_User);
         Auth.Set_Email (Cur_Email);
         Auth.Set_Method (AUTH_HASH_SHA1);
         Auth.Set_Ident (Email_Address);
      end if;

      --  Make a random salt and generate the password hash.
      Auth.Set_Salt (Model.Create_Key (Cur_User.Get_Id));
      Auth.Set_Hash
        (User_Service'Class (Model).Get_Password_Hash (Auth.Get_Salt, Password));
      Auth.Save (DB);

      Cur_User.Set_Status (Models.USER_ENABLED);
      Cur_User.Save (DB);

      User_Ref (User) := Cur_User;
      Email_Ref (Email) := Cur_Email;
      User_Lifecycle.Notify_Create (Model, User);

      --  Create the authentication session.
      Create_Session (Model, DB, Session, User, Auth, IpAddr, Principal);

      --  Post the user creation event once the user is registered.
      declare
         Sec_Ctx : Security.Contexts.Security_Context;
         Event   : AWA.Events.Module_Event;
      begin
         Sec_Ctx.Set_Context (Manager   => Model.Permissions.all'Access,
                              Principal => Principal.all'Access);
         Event.Set_Parameter ("email", Email.Get_Email);
         Model.Send_Alert (User_Create_Event.Kind, User, Event);

         --  Post a second event to notify the access key was validated.
         --  Can be used by an invitation process.
         Event.Set_Parameter ("key", Key);
         Model.Send_Alert (User_Key_Validation_Event.Kind, User, Event);
      end;

      --  Invalidate the key at then end when the event are handled..
      Access_Key.Delete (DB);

      Ctx.Commit;
   end Create_User;

   --  ------------------------------
   --  Load the user and email address from the invitation key.
   --  ------------------------------
   procedure Load_User (Model     : in out User_Service;
                        User      : in out User_Ref'Class;
                        Email     : in out Email_Ref'Class;
                        Key       : in String) is
      pragma Unreferenced (Model);

      Ctx           : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB            : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Access_Key    : Access_Key_Ref;
      Query         : ADO.SQL.Query;
      Found         : Boolean;
   begin
      Log.Info ("Get user from key {1}", Key);

      --  Verify the access key validity.
      Query.Bind_Param (1, Key);
      Query.Set_Filter ("access_key = ?");
      Access_Key.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No access key {0}", Key);
         raise Not_Found with "No access key: " & Key;
      end if;
      User := Access_Key.Get_User;
      Email := User.Get_Email;
   end Load_User;

   --  ------------------------------
   --  Update the user status to enable/disable the user account.
   --  ------------------------------
   procedure Update_User (Model  : in out User_Service;
                          Email  : in String;
                          Status : in Models.Status_Type) is
      pragma Unreferenced (Model);

      Ctx      : constant ASC.Service_Context_Access := ASC.Current;
      DB       : Master_Session := ASC.Get_Master_Session (Ctx);
      User     : User_Ref;
      Query    : ADO.SQL.Query;
      Found    : Boolean;
   begin
      Log.Info ("Update user status {0} to {1}", Email, Status'Image);
      Ctx.Start;

      --  Find the user associated with the email address.
      Query.Set_Join ("INNER JOIN awa_email e ON e.user_id = o.id");
      Query.Set_Filter ("LOWER(e.email) = LOWER(?)");
      Query.Bind_Param (1, Email);
      User.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No user with email address {0}", Email);
         raise Not_Found with "No user registered under email: " & Email;
      end if;

      User.Set_Status (Status);
      User.Save (DB);
      Ctx.Commit;
   end Update_User;

   --  ------------------------------
   --  Verify the access key and retrieve the user associated with that key.
   --  Starts a new session associated with the given IP address.
   --  Raises Not_Found if the access key does not exist.
   --  ------------------------------
   procedure Verify_User (Model    : in User_Service;
                          Key      : in String;
                          IpAddr   : in String;
                          Principal : out Users.Principals.Principal_Access) is
      Ctx        : constant ASC.Service_Context_Access := ASC.Current;
      DB         : Master_Session := ASC.Get_Master_Session (Ctx);
      Query      : ADO.SQL.Query;
      Found      : Boolean;
      Access_Key : Access_Key_Ref;
      User       : User_Ref;
      Session    : Session_Ref;
      Email      : Email_Ref;
   begin
      Log.Info ("Verify user with key {0}", Key);

      if Key'Length = 0 then
         Log.Warn ("Empty access key is refused");
         raise Not_Found with "No access key: ''";
      end if;

      Ctx.Start;

      --  Find the user associated with the given key
      Query.Bind_Param (1, Key);
      Query.Set_Filter ("access_key = ?");
      Access_Key.Find (DB, Query, Found);
      if not Found then
         Log.Warn ("No access key {0}", Key);
         raise Not_Found with "No access key: " & Key;
      end if;

      User := User_Ref (Access_Key.Get_User);

      Email := Email_Ref (User.Get_Email);

      --  Reject reset password disabled the user account.
      if User.Get_Status = Models.USER_DISABLED then
         Log.Warn ("User account {0} is disabled", Email.Get_Email);
         raise User_Disabled with "User account is disabled";
      end if;

      --  If the user is registered and has a password, it is now verified
      --  and we can enable it.
      declare
         Auth : Authenticate_Ref;
      begin
         Query.Clear;
         Query.Set_Filter ("o.method = 0 AND o.email_id = ?");
         Query.Bind_Param (1, Email.Get_Id);
         Auth.Find (DB, Query, Found);
         if User.Get_Status = Models.USER_REGISTERED and then Found then
            User.Set_Status (Models.USER_ENABLED);
            User.Save (DB);
         end if;

         User_Lifecycle.Notify_Create (Model, User);
         Principal := null;

         --  This account has no password, keep the key so that we can
         --  redirect to the change password page.  Otherwise, we must
         --  remove the access key and create the authenticate session.
         if Found then
            Access_Key.Delete (DB);

            --  Create the authentication session.
            Create_Session (Model, DB, Session, User, Auth, IpAddr, Principal);

            --  Post the user creation event once the user is registered.
            declare
               Sec_Ctx : Security.Contexts.Security_Context;
               Event   : AWA.Events.Module_Event;
            begin
               Sec_Ctx.Set_Context (Manager   => Model.Permissions.all'Access,
                                    Principal => Principal.all'Access);
               Event.Set_Parameter ("email", Email.Get_Email);
               Model.Send_Alert (User_Create_Event.Kind, User, Event);
            end;
         end if;
      end;
      Ctx.Commit;

   exception
      when ADO.Objects.NOT_FOUND =>
         Log.Warn ("No user linked to access key {0}", Key);
         raise Not_Found with "No user for access key: " & Key;

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

      Query.Set_Filter ("id = ? AND end_date IS NULL");
      Query.Bind_Param (1, Id);
      Session.Find (Session => DB,
                    Query   => Query,
                    Found   => Found);
      if not Found then
         Log.Warn ("Session {0} is not found or closed", Sid);
         raise Not_Found with "Session not found: " & Sid;
      end if;

      User := Session.Get_User;
   end Verify_Session;

   --  ------------------------------
   --  Closes the session identified by <b>Id</b>.  The session identified should refer to
   --  a valid and not closed connection session.
   --  When <b>Logout</b> is set, the authenticate session is also closed.  The connection
   --  sessions associated with the authenticate session are also closed.
   --  Raises <b>Not_Found</b> if the session is invalid or already closed.
   --  ------------------------------
   procedure Close_Session (Model  : in User_Service;
                            Id     : in ADO.Identifier;
                            Logout : in Boolean := False) is
      pragma Unreferenced (Model);

      Sid     : constant String := ADO.Identifier'Image (Id);
      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB      : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
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

      --  When logging out, close the authenticate session.
      if Logout then
         declare
            Auth_Session : Session_Ref'Class := Session.Get_Auth;
         begin
            Auth_Session.Set_End_Date (Session.Get_End_Date);
            Auth_Session.Save (DB);
            Session := Session_Ref (Auth_Session);
         end;
      end if;

      --  When closing the authenticate session, close any connection session that is still open.
      if Session.Get_Stype = Users.Models.AUTH_SESSION then
         declare
            Stmt : ADO.Statements.Update_Statement
              := DB.Create_Statement (AWA.Users.Models.SESSION_TABLE);
         begin
            Stmt.Save_Field (Name => "end_date", Value => Session.Get_End_Date);
            Stmt.Set_Filter ("auth_id = :auth AND end_date IS NULL");
            Stmt.Bind_Param ("auth", Session.Get_Id);
            Stmt.Execute;
         end;
      end if;
      Ctx.Commit;
   end Close_Session;

   --  ------------------------------
   --  Allow to disable the user registration.
   --  ------------------------------
   procedure Set_Allow_Register (Model : in out User_Service;
                                 Allow : in Boolean) is
   begin
      Model.Allow_Register := Allow;
   end Set_Allow_Register;

   --  ------------------------------
   --  Initialize the user service.
   --  ------------------------------
   overriding
   procedure Initialize (Model  : in out User_Service;
                         Module : in AWA.Modules.Module'Class) is
      DEFAULT_KEY : constant String := "8ef60aad66977c68b12f4f8acab5a4e00a77f6e8";
      Sec_Manager : constant Security.Policies.Policy_Manager_Access
         := Module.Get_Application.Get_Security_Manager;
   begin
      AWA.Modules.Module_Manager (Model).Initialize (Module);

      Model.Permissions := Permissions.Services.Permission_Manager'Class (Sec_Manager.all)'Access;
      Model.Server_Id := Module.Get_Config ("server_id", 1);
      Model.Allow_Register := Module.Get_Config ("allow_register", True);
      Set_Unbounded_String (Model.Auth_Key,
                            Module.Get_Config ("auth_key", DEFAULT_KEY));
      if Model.Auth_Key = DEFAULT_KEY then
         Log.Error ("The 'auth_key' configuration property not found.  Using default key.");
      end if;

      Log.Info ("User server associated with server id{0}", Integer'Image (Model.Server_Id));

      --  Close the connection sessions that have not been closed correctly.
      declare
         DB   : ADO.Sessions.Master_Session := Module.Get_Master_Session;
         Stmt : ADO.Statements.Update_Statement
           := DB.Create_Statement (AWA.Users.Models.SESSION_TABLE);
      begin
         DB.Begin_Transaction;
         Stmt.Save_Field (Name => "end_date",
                          Value => ADO.Nullable_Time '(Value   => Ada.Calendar.Clock,
                                                       Is_Null => False));
         Stmt.Set_Filter ("server_id = :server AND end_date IS NULL AND stype = :type");
         Stmt.Bind_Param ("server", Model.Server_Id);
         Stmt.Bind_Param ("type", CONNECT_SESSION_TYPE);
         Stmt.Execute;
         DB.Commit;
      end;
   end Initialize;

end AWA.Users.Services;
