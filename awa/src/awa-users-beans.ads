-----------------------------------------------------------------------
--  awa-users-beans -- ASF Beans for user module
--  Copyright (C) 2011, 2012, 2013, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;
with Util.Beans.Objects;
with Ada.Strings.Unbounded;

with AWA.Users.Services;
with AWA.Users.Modules;
with AWA.Users.Principals;
with AWA.Users.Models;

--  == Ada Beans ==
--  Several bean types are provided to represent and manage the users.
--  The user module registers the bean constructors when it is initialized.
--  To use them, one must declare a bean definition in the application
--  XML configuration.
--
--  @include-bean users.xml
--
package AWA.Users.Beans is

   use Ada.Strings.Unbounded;

   type Authenticate_Bean is new AWA.Users.Models.Authenticate_Bean with record
      Module     : AWA.Users.Modules.User_Module_Access := null;
      Manager    : AWA.Users.Services.User_Service_Access := null;
      Email      : Unbounded_String;
      Password   : Unbounded_String;
      First_Name : Unbounded_String;
      Last_Name  : Unbounded_String;
      Access_Key : Unbounded_String;
      Redirect   : Unbounded_String;
   end record;

   --  Attributes exposed by the <b>Authenticate_Bean</b> through Get_Value.
   EMAIL_ATTR      : constant String := "email";
   PASSWORD_ATTR   : constant String := "password";
   FIRST_NAME_ATTR : constant String := "firstName";
   LAST_NAME_ATTR  : constant String := "lastName";
   KEY_ATTR        : constant String := "key";
   REDIRECT_ATTR   : constant String := "redirect";

   type Authenticate_Bean_Access is access all Authenticate_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Authenticate_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Authenticate_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Set_Session_Principal (Data      : in Authenticate_Bean;
                                    Principal : in AWA.Users.Principals.Principal_Access);

   procedure Set_Authenticate_Cookie (Data      : in out Authenticate_Bean;
                                      Principal : in AWA.Users.Principals.Principal_Access);

   --  Action to register a user
   overriding
   procedure Register (Data    : in out Authenticate_Bean;
                       Outcome : in out Unbounded_String);

   --  Action to trigger the lost password email process.
   overriding
   procedure Lost_Password (Data    : in out Authenticate_Bean;
                            Outcome : in out Unbounded_String);

   --  Action to validate the reset password key and set a new password.
   overriding
   procedure Reset_Password (Data    : in out Authenticate_Bean;
                             Outcome : in out Unbounded_String);

   --  Action to authenticate a user (password authentication).
   overriding
   procedure Authenticate (Data    : in out Authenticate_Bean;
                           Outcome : in out Unbounded_String);

   --  Logout the user and closes the session.
   overriding
   procedure Logout (Data    : in out Authenticate_Bean;
                     Outcome : in out Unbounded_String);

   overriding
   procedure Load (Data    : in out Authenticate_Bean;
                   Outcome : in out Unbounded_String);

   overriding
   procedure Auth_Error (Data    : in out Authenticate_Bean;
                         Outcome : in out Unbounded_String);

   --  Create an authenticate bean.
   function Create_Authenticate_Bean (Module : in AWA.Users.Modules.User_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Current user
   --  ------------------------------
   --  The <b>Current_User_Bean</b> provides information about the current user.
   --  It relies on the <b>AWA.Services.Contexts</b> to identify the current user
   --  and return information about him/her.
   type Current_User_Bean is new Util.Beans.Basic.Readonly_Bean with null record;
   type Current_User_Bean_Access is access all Current_User_Bean'Class;

   --  Attributes exposed by <b>Current_User_Bean</b>
   IS_LOGGED_ATTR  : constant String := "isLogged";
   USER_EMAIL_ATTR : constant String := "email";
   USER_NAME_ATTR  : constant String := "name";
   USER_ID_ATTR    : constant String := "id";

   --  Get the value identified by the name.  The following names are recognized:
   --    o isLogged  Boolean  True if a user is logged
   --    o name      String   The user name
   --    o email     String   The user email address
   --    o id        Long     The user identifier
   overriding
   function Get_Value (From : in Current_User_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Create the current user bean.
   function Create_Current_User_Bean (Module : in AWA.Users.Modules.User_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Users.Beans;
