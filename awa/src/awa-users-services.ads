-----------------------------------------------------------------------
--  awa.users -- User registration, authentication processes
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2017, 2018, 2022 Stephane Carrez
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

with AWA.Users.Models;
with AWA.Users.Principals;
with AWA.Permissions.Services;
with AWA.Modules;
with AWA.Modules.Lifecycles;
with AWA.Events;
with Security.Auth;
with Security.Random;

with ADO;
with ADO.Sessions;

--  == Introduction ==
--  The *users* module provides a *users* service which controls the user data model.
--
--  == Events ==
--  The *users* module exposes a number of events which are posted when some action
--  are performed at the service level.
--
--  === user-register ===
--  This event is posted when a new user is registered in the application.
--  It can be used to send a registration email.
--
--  === user-create ===
--  This event is posted when a new user is created.  It can be used to trigger
--  the pre-initialization of the application for the new user.
--
--  === user-lost-password ===
--  This event is posted when a user asks for a password reset through an
--  anonymous form.  It is intended to be used to send the reset password email.
--
--  === user-reset-password ===
--  This event is posted when a user has successfully reset his password.
--  It can be used to send an email.
--
package AWA.Users.Services is

   use AWA.Users.Models;

   package User_Create_Event is new AWA.Events.Definition (Name => "user-create");
   package User_Register_Event is new AWA.Events.Definition (Name => "user-register");
   package User_Lost_Password_Event is new AWA.Events.Definition (Name => "user-lost-password");
   package User_Reset_Password_Event is new AWA.Events.Definition (Name => "user-reset-password");

   package User_Lifecycle is
     new AWA.Modules.Lifecycles (Element_Type => AWA.Users.Models.User_Ref'Class);

   subtype Listener is User_Lifecycle.Listener;

   NAME : constant String := "User_Service";

   Not_Found  : exception;

   User_Exist : exception;

   User_Disabled : exception;

   Registration_Disabled : exception;

   --  The session is an authenticate session.  The user authenticated using password or OpenID.
   --  When the user logout, this session is closed as well as any connection session linked to
   --  the authenticate session.
   AUTH_SESSION_TYPE    : constant Integer := 1;

   --  The session is a connection session.  It is linked to an authenticate session.
   --  This session can be closed automatically due to a timeout or user's inactivity.
   --  The AID cookie refers to this connection session to create a new connection session.
   --  Once re-connecting is done, the connection session refered to by AID is marked
   --  as <b<USED_SESSION_TYPE</b> and a new AID cookie with the new connection session is
   --  returned to the user.
   CONNECT_SESSION_TYPE : constant Integer := 0;

   --  The session is a connection session whose associated AID cookie has been used.
   --  This session cannot be used to re-connect a user through the AID cookie.
   USED_SESSION_TYPE    : constant Integer := 2;

   --  Get the user name from the email address.
   --  Returns the possible user name from his email address.
   function Get_Name_From_Email (Email : in String) return String;

   type User_Service is new AWA.Modules.Module_Manager with private;
   type User_Service_Access is access all User_Service'Class;

   --  Build the authenticate cookie.  The cookie is signed using HMAC-SHA1 with a private key.
   function Get_Authenticate_Cookie (Model : in User_Service;
                                     Id    : in ADO.Identifier)
                                     return String;

   --  Get the authenticate identifier from the cookie.
   --  Verify that the cookie is valid, the signature is correct.
   --  Returns the identified or NO_IDENTIFIER
   function Get_Authenticate_Id (Model  : in User_Service;
                                 Cookie : in String) return ADO.Identifier;

   --  Get the password hash.  The password is signed using HMAC-SHA1 with the salt.
   function Get_Password_Hash (Model    : in User_Service;
                               Password : in String;
                               Salt     : in String)
                              return String;

   --  Create a user in the database with the given user information and
   --  the associated email address.  Verify that no such user already exist.
   --  Build an access key that allows to verify the user email and finish
   --  the account creation.
   --  Raises User_Exist exception if a user with such email is already registered.
   procedure Create_User (Model    : in out User_Service;
                          User     : in out User_Ref'Class;
                          Email    : in out Email_Ref'Class;
                          Password : in String;
                          Key      : in out Access_Key_Ref'Class;
                          Send     : in Boolean);

   --  Create a user in the database with the given user information and
   --  the associated email address and for the given access key.  The access key is first
   --  verified and the user instance associated with it is retrieved.  Verify that the email
   --  address is unique and can be used by the user.  Since the access key is verified,
   --  grant immediately the access by opening a session and setting up the principal instance.
   --  Raises User_Exist exception if a user with such email is already registered.
   procedure Create_User (Model     : in out User_Service;
                          User      : in out User_Ref'Class;
                          Email     : in out Email_Ref'Class;
                          Password  : in String;
                          Key       : in String;
                          IpAddr    : in String;
                          Principal : out AWA.Users.Principals.Principal_Access);

   --  Load the user and email address from the invitation key.
   procedure Load_User (Model     : in out User_Service;
                        User      : in out User_Ref'Class;
                        Email     : in out Email_Ref'Class;
                        Key       : in String);

   --  Update the user status to enable/disable the user account.
   procedure Update_User (Model  : in out User_Service;
                          Email  : in String;
                          Status : in Models.Status_type);

   --  Verify the access key and retrieve the user associated with that key.
   --  Starts a new session associated with the given IP address.
   --  The authenticated user is identified by a principal instance allocated
   --  and returned in <b>Principal</b>.
   --  Raises Not_Found if the access key does not exist.
   procedure Verify_User (Model     : in User_Service;
                          Key       : in String;
                          IpAddr    : in String;
                          Principal : out AWA.Users.Principals.Principal_Access);

   --  Authenticate the user with his email address and his password.
   --  If the user is authenticated, return the user information and
   --  create a new session.  The IP address of the connection is saved
   --  in the session.
   --  Raises Not_Found exception if the user is not recognized
   procedure Authenticate (Model     : in User_Service;
                           Email     : in String;
                           Password  : in String;
                           IpAddr    : in String;
                           Principal : out AWA.Users.Principals.Principal_Access);

   --  Authenticate the user with his OpenID identifier.  The authentication process
   --  was made by an external OpenID provider.  If the user does not yet exists in
   --  the database, a record is created for him.  Create a new session for the user.
   --  The IP address of the connection is saved in the session.
   --  Raises Not_Found exception if the user is not recognized
   procedure Authenticate (Model     : in User_Service;
                           Auth      : in Security.Auth.Authentication;
                           IpAddr    : in String;
                           Principal : out AWA.Users.Principals.Principal_Access);

   --  Authenticate the user with the authenticate cookie generated from a previous authenticate
   --  session.  If the cookie has the correct signature, matches a valid session,
   --  return the user information and create a new session.  The IP address of the connection
   --  is saved in the session.
   --  Raises Not_Found exception if the user is not recognized
   procedure Authenticate (Model     : in User_Service;
                           Cookie    : in String;
                           Ip_Addr   : in String;
                           Principal : out AWA.Users.Principals.Principal_Access);

   --  Start the lost password process for a user.  Find the user having
   --  the given email address and send that user a password reset key
   --  in an email.
   --  Raises Not_Found exception if no user with such email exist
   procedure Lost_Password (Model : in out User_Service;
                            Email : in String);

   --  Reset the password of the user associated with the secure key.
   --  Raises Not_Found if there is no key or if the user does not have any email
   procedure Reset_Password (Model    : in out User_Service;
                             Key      : in String;
                             Password : in String;
                             IpAddr   : in String;
                             Principal : out AWA.Users.Principals.Principal_Access);

   --  Verify that the user session identified by <b>Id</b> is valid and still active.
   --  Returns the user and the session objects.
   --  Raises Not_Found if the session does not exist or was closed.
   procedure Verify_Session (Model   : in User_Service;
                             Id      : in ADO.Identifier;
                             User    : out User_Ref'Class;
                             Session : out Session_Ref'Class);

   --  Closes the session identified by <b>Id</b>.  The session identified should refer to
   --  a valid and not closed connection session.
   --  When <b>Logout</b> is set, the authenticate session is also closed.  The connection
   --  sessions associated with the authenticate session are also closed.
   --  Raises <b>Not_Found</b> if the session is invalid or already closed.
   procedure Close_Session (Model  : in User_Service;
                            Id     : in ADO.Identifier;
                            Logout : in Boolean := False);

   --  Create and generate a new access key for the user.  The access key is saved in the
   --  database and it will expire after the expiration delay.
   procedure Create_Access_Key (Model   : in out User_Service;
                                User    : in AWA.Users.Models.User_Ref'Class;
                                Key     : in out AWA.Users.Models.Access_Key_Ref;
                                Kind    : in AWA.Users.Models.Key_Type;
                                Expire  : in Duration;
                                Session : in out ADO.Sessions.Master_Session);

   procedure Send_Alert (Model : in User_Service;
                         Kind  : in AWA.Events.Event_Index;
                         User  : in User_Ref'Class;
                         Props : in out AWA.Events.Module_Event);

   --  Allow to disable the user registration.
   procedure Set_Allow_Register (Model : in out User_Service;
                                 Allow : in Boolean);

   --  Initialize the user service.
   overriding
   procedure Initialize (Model  : in out User_Service;
                         Module : in AWA.Modules.Module'Class);

private

   function Create_Key (Model  : in out User_Service;
                        Number : in ADO.Identifier) return String;

   procedure Create_Session (Model   : in User_Service;
                             DB      : in out ADO.Sessions.Master_Session;
                             Session : out Session_Ref'Class;
                             User    : in User_Ref'Class;
                             Auth    : in Authenticate_Ref'Class;
                             Ip_Addr : in String;
                             Principal : out AWA.Users.Principals.Principal_Access) with
     Pre => (User.Is_Loaded or else User.Is_Inserted)
     and then (Auth.Is_Loaded or else Auth.Is_Inserted);

   type User_Service is new AWA.Modules.Module_Manager with record
      Server_Id   : Integer := 0;
      Random      : Security.Random.Generator;
      Auth_Key    : Ada.Strings.Unbounded.Unbounded_String;
      Permissions : AWA.Permissions.Services.Permission_Manager_Access;
      Allow_Register : Boolean := True;
   end record;

end AWA.Users.Services;
