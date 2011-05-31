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
with AWA.Users.Models;
with AWA.Modules;
with ASF.Events.Modules;
with ADO;
package AWA.Users.Services is

   use AWA.Users.Models;

   NAME : constant String := "User_Manager";

   Not_Found  : exception;

   User_Exist : exception;

   type User_Manager is new AWA.Modules.Module_Manager with private;
   type User_Manager_Access is access all User_Manager'Class;

   --  Create a user in the database with the given user information and
   --  the associated email address.  Verify that no such user already exist.
   --  Raises User_Exist exception if a user with such email is already registered.
   procedure Create_User (Model : in User_Manager;
                          User  : in out User_Ref'Class;
                          Email : in out Email_Ref'Class);

   --  Verify the access key and retrieve the user associated with that key.
   --  Starts a new session associated with the given IP address.
   --  Raises Not_Found if the access key does not exist.
   procedure Verify_User (Model    : in User_Manager;
                          Key      : in String;
                          IpAddr   : in String;
                          User     : out User_Ref'Class;
                          Session  : out Session_Ref'Class);

   --  Authenticate the user with his email address and his password.
   --  If the user is authenticated, return the user information and
   --  create a new session.  The IP address of the connection is saved
   --  in the session.
   --  Raises Not_Found exception if the user is not recognized
   procedure Authenticate (Model    : in User_Manager;
                           Email    : in String;
                           Password : in String;
                           IpAddr   : in String;
                           User     : out User_Ref'Class;
                           Session  : out Session_Ref'Class);

   --  Authenticate the user with his open id identifier.  The authentication process
   --  was made by an external OpenID provider.  If the user does not yet exists in
   --  the database, a record is created for him.  Create a new session for the user.
   --  The IP address of the connection is saved in the session.
   --  Raises Not_Found exception if the user is not recognized
   procedure Authenticate (Model    : in User_Manager;
                           OpenId   : in String;
                           Email    : in String;
                           Name     : in String;
                           IpAddr   : in String;
                           User     : out User_Ref'Class;
                           Session  : out Session_Ref'Class);

   --  Start the lost password process for a user.  Find the user having
   --  the given email address and send that user a password reset key
   --  in an email.
   --  Raises Not_Found exception if no user with such email exist
   procedure Lost_Password (Model : in User_Manager;
                            Email : in String);

   --  Reset the password of the user associated with the secure key.
   --  to the user in an email.
   --  Raises Not_Found if there is no key or if the user does not have any email
   procedure Reset_Password (Model    : in User_Manager;
                             Key      : in String;
                             Password : in String;
                             IpAddr   : in String;
                             User     : out User_Ref'Class;
                             Session  : out Session_Ref'Class);

   --  Verify that the user session identified by <b>Id</b> is valid and still active.
   --  Returns the user and the session objects.
   --  Raises Not_Found if the session does not exist or was closed.
   procedure Verify_Session (Model   : in User_Manager;
                             Id      : in ADO.Identifier;
                             User    : out User_Ref'Class;
                             Session : out Session_Ref'Class);

   --  Closes the session identified by <b>Id</b>.
   procedure Close_Session (Model : in User_Manager;
                            Id    : in ADO.Identifier);

   procedure Send_Alert (Model : in User_Manager;
                         Name  : in String;
                         User  : in User_Ref'Class;
                         Props : in out ASF.Events.Modules.Module_Event);

private

   type User_Manager is new AWA.Modules.Module_Manager with null record;

end AWA.Users.Services;
