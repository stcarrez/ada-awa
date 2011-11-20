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
with Ada.Strings.Unbounded;

with AWA.Users.Models;
with AWA.Modules;
with ASF.Events.Modules;
with Security.Openid;
with ADO;
package AWA.Users.Services is

   use AWA.Users.Models;

   NAME : constant String := "User_Service";

   Not_Found  : exception;

   User_Exist : exception;

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

   --  Create a user in the database with the given user information and
   --  the associated email address.  Verify that no such user already exist.
   --  Raises User_Exist exception if a user with such email is already registered.
   procedure Create_User (Model : in User_Service;
                          User  : in out User_Ref'Class;
                          Email : in out Email_Ref'Class);

   --  Verify the access key and retrieve the user associated with that key.
   --  Starts a new session associated with the given IP address.
   --  Raises Not_Found if the access key does not exist.
   procedure Verify_User (Model    : in User_Service;
                          Key      : in String;
                          IpAddr   : in String;
                          User     : out User_Ref'Class;
                          Session  : out Session_Ref'Class);

   --  Authenticate the user with his email address and his password.
   --  If the user is authenticated, return the user information and
   --  create a new session.  The IP address of the connection is saved
   --  in the session.
   --  Raises Not_Found exception if the user is not recognized
   procedure Authenticate (Model    : in User_Service;
                           Email    : in String;
                           Password : in String;
                           IpAddr   : in String;
                           User     : out User_Ref'Class;
                           Session  : out Session_Ref'Class);

   --  Authenticate the user with his OpenID identifier.  The authentication process
   --  was made by an external OpenID provider.  If the user does not yet exists in
   --  the database, a record is created for him.  Create a new session for the user.
   --  The IP address of the connection is saved in the session.
   --  Raises Not_Found exception if the user is not recognized
   procedure Authenticate (Model    : in User_Service;
                           Auth     : in Security.Openid.Authentication;
                           IpAddr   : in String;
                           User     : out User_Ref'Class;
                           Session  : out Session_Ref'Class);

   --  Authenticate the user with the authenticate cookie generated from a previous authenticate
   --  session.  If the cookie has the correct signature, matches a valid session,
   --  return the user information and create a new session.  The IP address of the connection
   --  is saved in the session.
   --  Raises Not_Found exception if the user is not recognized
   procedure Authenticate (Model    : in User_Service;
                           Cookie   : in String;
                           Ip_Addr  : in String;
                           User     : out User_Ref'Class;
                           Session  : out Session_Ref'Class);

   --  Start the lost password process for a user.  Find the user having
   --  the given email address and send that user a password reset key
   --  in an email.
   --  Raises Not_Found exception if no user with such email exist
   procedure Lost_Password (Model : in User_Service;
                            Email : in String);

   --  Reset the password of the user associated with the secure key.
   --  to the user in an email.
   --  Raises Not_Found if there is no key or if the user does not have any email
   procedure Reset_Password (Model    : in User_Service;
                             Key      : in String;
                             Password : in String;
                             IpAddr   : in String;
                             User     : out User_Ref'Class;
                             Session  : out Session_Ref'Class);

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

   procedure Send_Alert (Model : in User_Service;
                         Name  : in String;
                         User  : in User_Ref'Class;
                         Props : in out ASF.Events.Modules.Module_Event);

   --  Initialize the user service.
   overriding
   procedure Initialize (Model  : in out User_Service;
                         Module : in AWA.Modules.Module'Class);

private

   type User_Service is new AWA.Modules.Module_Manager with record
      Server_Id : Integer := 0;
      Auth_Key  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end AWA.Users.Services;
