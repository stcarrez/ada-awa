-----------------------------------------------------------------------
--  AWA.Users.Models -- AWA.Users.Models
-----------------------------------------------------------------------
--  File generated by ada-gen DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://ada-gen.googlecode.com/svn/trunk Revision 1095
-----------------------------------------------------------------------
--  Copyright (C) 2018 Stephane Carrez
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
pragma Warnings (Off);
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Objects.Enums;
with Util.Beans.Basic.Lists;
pragma Warnings (On);
package AWA.Users.Models is

   pragma Style_Checks ("-mr");

   type Key_Type is (RESET_PASSWORD_KEY, SIGNUP_KEY, INVITATION_KEY);
   for Key_Type use (RESET_PASSWORD_KEY => 0, SIGNUP_KEY => 1, INVITATION_KEY => 2);
   package Key_Type_Objects is
      new Util.Beans.Objects.Enums (Key_Type);

   type MailDeliveryStatus is (UNKNOWN, VALID, SOFT_BOUNCE, HARD_BOUNCE);
   for MailDeliveryStatus use (UNKNOWN => 0, VALID => 1, SOFT_BOUNCE => 2, HARD_BOUNCE => 3);
   package MailDeliveryStatus_Objects is
      new Util.Beans.Objects.Enums (MailDeliveryStatus);

   --  --------------------
   --  The Auth_Session is created when a user is authentified.
   --  The Connect_Session is created each time the user establishes a session on
   --  the application. The Connect_Session is always associated with an Auth_Session.
   --  --------------------
   type Session_Type is (CONNECT_SESSION, AUTH_SESSION, USED_SESSION);
   for Session_Type use (CONNECT_SESSION => 0, AUTH_SESSION => 1, USED_SESSION => 2);
   package Session_Type_Objects is
      new Util.Beans.Objects.Enums (Session_Type);

   type Email_Ref is new ADO.Objects.Object_Ref with null record;

   type User_Ref is new ADO.Objects.Object_Ref with null record;

   type Access_Key_Ref is new ADO.Objects.Object_Ref with null record;

   type Session_Ref is new ADO.Objects.Object_Ref with null record;

   --  --------------------
   --  The Email entity defines the user email addresses.
   --  The user has a primary email address that is obtained
   --  from the registration process (either through a form
   --  submission or through OpenID authentication).
   --  --------------------
   --  Create an object key for Email.
   function Email_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Email from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Email_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Email : constant Email_Ref;
   function "=" (Left, Right : Email_Ref'Class) return Boolean;

   --  Set the email address.
   procedure Set_Email (Object : in out Email_Ref;
                        Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Email (Object : in out Email_Ref;
                        Value : in String);

   --  Get the email address.
   function Get_Email (Object : in Email_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Email (Object : in Email_Ref)
                 return String;

   --  Set the last mail delivery status (if known).
   procedure Set_Status (Object : in out Email_Ref;
                         Value  : in AWA.Users.Models.MailDeliveryStatus);

   --  Get the last mail delivery status (if known).
   function Get_Status (Object : in Email_Ref)
                 return AWA.Users.Models.MailDeliveryStatus;

   --  Set the date when the last email error was detected.
   procedure Set_Last_Error_Date (Object : in out Email_Ref;
                                  Value  : in Ada.Calendar.Time);

   --  Get the date when the last email error was detected.
   function Get_Last_Error_Date (Object : in Email_Ref)
                 return Ada.Calendar.Time;
   --
   function Get_Version (Object : in Email_Ref)
                 return Integer;

   --  Set the email primary key.
   procedure Set_Id (Object : in out Email_Ref;
                     Value  : in ADO.Identifier);

   --  Get the email primary key.
   function Get_Id (Object : in Email_Ref)
                 return ADO.Identifier;

   --  Set the user.
   procedure Set_User_Id (Object : in out Email_Ref;
                          Value  : in ADO.Identifier);

   --  Get the user.
   function Get_User_Id (Object : in Email_Ref)
                 return ADO.Identifier;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Email_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Email_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Email_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Email_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Email_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Email_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   EMAIL_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Email_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Email_Ref;
                   Into   : in out Email_Ref);

   --  --------------------
   --  The User entity represents a user that can access and use the application.
   --  --------------------
   --  Create an object key for User.
   function User_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for User from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function User_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_User : constant User_Ref;
   function "=" (Left, Right : User_Ref'Class) return Boolean;

   --  Set the user first name.
   procedure Set_First_Name (Object : in out User_Ref;
                             Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_First_Name (Object : in out User_Ref;
                             Value : in String);

   --  Get the user first name.
   function Get_First_Name (Object : in User_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_First_Name (Object : in User_Ref)
                 return String;

   --  Set the user last name.
   procedure Set_Last_Name (Object : in out User_Ref;
                            Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Last_Name (Object : in out User_Ref;
                            Value : in String);

   --  Get the user last name.
   function Get_Last_Name (Object : in User_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Last_Name (Object : in User_Ref)
                 return String;

   --  Set the user password hash.
   procedure Set_Password (Object : in out User_Ref;
                           Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Password (Object : in out User_Ref;
                           Value : in String);

   --  Get the user password hash.
   function Get_Password (Object : in User_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Password (Object : in User_Ref)
                 return String;

   --  Set the user OpenID identifier.
   procedure Set_Open_Id (Object : in out User_Ref;
                          Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Open_Id (Object : in out User_Ref;
                          Value : in String);

   --  Get the user OpenID identifier.
   function Get_Open_Id (Object : in User_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Open_Id (Object : in User_Ref)
                 return String;

   --  Set the user country.
   procedure Set_Country (Object : in out User_Ref;
                          Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Country (Object : in out User_Ref;
                          Value : in String);

   --  Get the user country.
   function Get_Country (Object : in User_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Country (Object : in User_Ref)
                 return String;

   --  Set the user display name.
   procedure Set_Name (Object : in out User_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out User_Ref;
                       Value : in String);

   --  Get the user display name.
   function Get_Name (Object : in User_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in User_Ref)
                 return String;
   --  Get version number.
   function Get_Version (Object : in User_Ref)
                 return Integer;

   --  Set the user identifier.
   procedure Set_Id (Object : in out User_Ref;
                     Value  : in ADO.Identifier);

   --  Get the user identifier.
   function Get_Id (Object : in User_Ref)
                 return ADO.Identifier;

   --  Set the password salt.
   procedure Set_Salt (Object : in out User_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Salt (Object : in out User_Ref;
                       Value : in String);

   --  Get the password salt.
   function Get_Salt (Object : in User_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Salt (Object : in User_Ref)
                 return String;

   --
   procedure Set_Email (Object : in out User_Ref;
                        Value  : in AWA.Users.Models.Email_Ref'Class);

   --
   function Get_Email (Object : in User_Ref)
                 return AWA.Users.Models.Email_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out User_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out User_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out User_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out User_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out User_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in User_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   USER_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out User_Ref);

   --  Copy of the object.
   procedure Copy (Object : in User_Ref;
                   Into   : in out User_Ref);

   --  Create an object key for Access_Key.
   function Access_Key_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Access_Key from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Access_Key_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Access_Key : constant Access_Key_Ref;
   function "=" (Left, Right : Access_Key_Ref'Class) return Boolean;

   --  Set the secure access key.
   procedure Set_Access_Key (Object : in out Access_Key_Ref;
                             Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Access_Key (Object : in out Access_Key_Ref;
                             Value : in String);

   --  Get the secure access key.
   function Get_Access_Key (Object : in Access_Key_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Access_Key (Object : in Access_Key_Ref)
                 return String;

   --  Set the access key expiration date.
   procedure Set_Expire_Date (Object : in out Access_Key_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the access key expiration date.
   function Get_Expire_Date (Object : in Access_Key_Ref)
                 return Ada.Calendar.Time;

   --  Set the access key identifier.
   procedure Set_Id (Object : in out Access_Key_Ref;
                     Value  : in ADO.Identifier);

   --  Get the access key identifier.
   function Get_Id (Object : in Access_Key_Ref)
                 return ADO.Identifier;
   --
   function Get_Version (Object : in Access_Key_Ref)
                 return Integer;

   --  Set the access key type.
   procedure Set_Kind (Object : in out Access_Key_Ref;
                       Value  : in AWA.Users.Models.Key_Type);

   --  Get the access key type.
   function Get_Kind (Object : in Access_Key_Ref)
                 return AWA.Users.Models.Key_Type;

   --
   procedure Set_User (Object : in out Access_Key_Ref;
                       Value  : in AWA.Users.Models.User_Ref'Class);

   --
   function Get_User (Object : in Access_Key_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Access_Key_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Access_Key_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Access_Key_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Access_Key_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Access_Key_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Access_Key_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   ACCESS_KEY_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Access_Key_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Access_Key_Ref;
                   Into   : in out Access_Key_Ref);

   --  Create an object key for Session.
   function Session_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Session from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Session_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Session : constant Session_Ref;
   function "=" (Left, Right : Session_Ref'Class) return Boolean;

   --
   procedure Set_Start_Date (Object : in out Session_Ref;
                             Value  : in Ada.Calendar.Time);

   --
   function Get_Start_Date (Object : in Session_Ref)
                 return Ada.Calendar.Time;

   --
   procedure Set_End_Date (Object : in out Session_Ref;
                           Value  : in ADO.Nullable_Time);

   --
   function Get_End_Date (Object : in Session_Ref)
                 return ADO.Nullable_Time;

   --
   procedure Set_Ip_Address (Object : in out Session_Ref;
                             Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Ip_Address (Object : in out Session_Ref;
                             Value : in String);

   --
   function Get_Ip_Address (Object : in Session_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Ip_Address (Object : in Session_Ref)
                 return String;

   --
   procedure Set_Stype (Object : in out Session_Ref;
                        Value  : in AWA.Users.Models.Session_Type);

   --
   function Get_Stype (Object : in Session_Ref)
                 return AWA.Users.Models.Session_Type;
   --
   function Get_Version (Object : in Session_Ref)
                 return Integer;

   --
   procedure Set_Server_Id (Object : in out Session_Ref;
                            Value  : in Integer);

   --
   function Get_Server_Id (Object : in Session_Ref)
                 return Integer;

   --
   procedure Set_Id (Object : in out Session_Ref;
                     Value  : in ADO.Identifier);

   --
   function Get_Id (Object : in Session_Ref)
                 return ADO.Identifier;

   --
   procedure Set_Auth (Object : in out Session_Ref;
                       Value  : in AWA.Users.Models.Session_Ref'Class);

   --
   function Get_Auth (Object : in Session_Ref)
                 return AWA.Users.Models.Session_Ref'Class;

   --
   procedure Set_User (Object : in out Session_Ref;
                       Value  : in AWA.Users.Models.User_Ref'Class);

   --
   function Get_User (Object : in Session_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Session_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Session_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Session_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Session_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Session_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Session_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   SESSION_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Session_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Session_Ref;
                   Into   : in out Session_Ref);




private
   EMAIL_NAME : aliased constant String := "awa_email";
   COL_0_1_NAME : aliased constant String := "email";
   COL_1_1_NAME : aliased constant String := "status";
   COL_2_1_NAME : aliased constant String := "last_error_date";
   COL_3_1_NAME : aliased constant String := "version";
   COL_4_1_NAME : aliased constant String := "id";
   COL_5_1_NAME : aliased constant String := "user_id";

   EMAIL_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 6,
      Table   => EMAIL_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access,
         6 => COL_5_1_NAME'Access)
     );
   EMAIL_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := EMAIL_DEF'Access;


   Null_Email : constant Email_Ref
      := Email_Ref'(ADO.Objects.Object_Ref with null record);

   type Email_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => EMAIL_DEF'Access)
   with record
       Email : Ada.Strings.Unbounded.Unbounded_String;
       Status : AWA.Users.Models.MailDeliveryStatus;
       Last_Error_Date : Ada.Calendar.Time;
       Version : Integer;
       User_Id : ADO.Identifier;
   end record;

   type Email_Access is access all Email_Impl;

   overriding
   procedure Destroy (Object : access Email_Impl);

   overriding
   procedure Find (Object  : in out Email_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Email_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Email_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Email_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Email_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Email_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Email_Ref'Class;
                        Impl   : out Email_Access);
   USER_NAME : aliased constant String := "awa_user";
   COL_0_2_NAME : aliased constant String := "first_name";
   COL_1_2_NAME : aliased constant String := "last_name";
   COL_2_2_NAME : aliased constant String := "password";
   COL_3_2_NAME : aliased constant String := "open_id";
   COL_4_2_NAME : aliased constant String := "country";
   COL_5_2_NAME : aliased constant String := "name";
   COL_6_2_NAME : aliased constant String := "version";
   COL_7_2_NAME : aliased constant String := "id";
   COL_8_2_NAME : aliased constant String := "salt";
   COL_9_2_NAME : aliased constant String := "email_id";

   USER_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 10,
      Table   => USER_NAME'Access,
      Members => (
         1 => COL_0_2_NAME'Access,
         2 => COL_1_2_NAME'Access,
         3 => COL_2_2_NAME'Access,
         4 => COL_3_2_NAME'Access,
         5 => COL_4_2_NAME'Access,
         6 => COL_5_2_NAME'Access,
         7 => COL_6_2_NAME'Access,
         8 => COL_7_2_NAME'Access,
         9 => COL_8_2_NAME'Access,
         10 => COL_9_2_NAME'Access)
     );
   USER_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := USER_DEF'Access;


   Null_User : constant User_Ref
      := User_Ref'(ADO.Objects.Object_Ref with null record);

   type User_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => USER_DEF'Access)
   with record
       First_Name : Ada.Strings.Unbounded.Unbounded_String;
       Last_Name : Ada.Strings.Unbounded.Unbounded_String;
       Password : Ada.Strings.Unbounded.Unbounded_String;
       Open_Id : Ada.Strings.Unbounded.Unbounded_String;
       Country : Ada.Strings.Unbounded.Unbounded_String;
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Version : Integer;
       Salt : Ada.Strings.Unbounded.Unbounded_String;
       Email : AWA.Users.Models.Email_Ref;
   end record;

   type User_Access is access all User_Impl;

   overriding
   procedure Destroy (Object : access User_Impl);

   overriding
   procedure Find (Object  : in out User_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out User_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out User_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out User_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out User_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out User_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out User_Ref'Class;
                        Impl   : out User_Access);
   ACCESS_KEY_NAME : aliased constant String := "awa_access_key";
   COL_0_3_NAME : aliased constant String := "access_key";
   COL_1_3_NAME : aliased constant String := "expire_date";
   COL_2_3_NAME : aliased constant String := "id";
   COL_3_3_NAME : aliased constant String := "version";
   COL_4_3_NAME : aliased constant String := "kind";
   COL_5_3_NAME : aliased constant String := "user_id";

   ACCESS_KEY_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 6,
      Table   => ACCESS_KEY_NAME'Access,
      Members => (
         1 => COL_0_3_NAME'Access,
         2 => COL_1_3_NAME'Access,
         3 => COL_2_3_NAME'Access,
         4 => COL_3_3_NAME'Access,
         5 => COL_4_3_NAME'Access,
         6 => COL_5_3_NAME'Access)
     );
   ACCESS_KEY_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := ACCESS_KEY_DEF'Access;


   Null_Access_Key : constant Access_Key_Ref
      := Access_Key_Ref'(ADO.Objects.Object_Ref with null record);

   type Access_Key_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => ACCESS_KEY_DEF'Access)
   with record
       Access_Key : Ada.Strings.Unbounded.Unbounded_String;
       Expire_Date : Ada.Calendar.Time;
       Version : Integer;
       Kind : AWA.Users.Models.Key_Type;
       User : AWA.Users.Models.User_Ref;
   end record;

   type Access_Key_Access is access all Access_Key_Impl;

   overriding
   procedure Destroy (Object : access Access_Key_Impl);

   overriding
   procedure Find (Object  : in out Access_Key_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Access_Key_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Access_Key_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Access_Key_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Access_Key_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Access_Key_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Access_Key_Ref'Class;
                        Impl   : out Access_Key_Access);
   SESSION_NAME : aliased constant String := "awa_session";
   COL_0_4_NAME : aliased constant String := "start_date";
   COL_1_4_NAME : aliased constant String := "end_date";
   COL_2_4_NAME : aliased constant String := "ip_address";
   COL_3_4_NAME : aliased constant String := "stype";
   COL_4_4_NAME : aliased constant String := "version";
   COL_5_4_NAME : aliased constant String := "server_id";
   COL_6_4_NAME : aliased constant String := "id";
   COL_7_4_NAME : aliased constant String := "auth_id";
   COL_8_4_NAME : aliased constant String := "user_id";

   SESSION_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 9,
      Table   => SESSION_NAME'Access,
      Members => (
         1 => COL_0_4_NAME'Access,
         2 => COL_1_4_NAME'Access,
         3 => COL_2_4_NAME'Access,
         4 => COL_3_4_NAME'Access,
         5 => COL_4_4_NAME'Access,
         6 => COL_5_4_NAME'Access,
         7 => COL_6_4_NAME'Access,
         8 => COL_7_4_NAME'Access,
         9 => COL_8_4_NAME'Access)
     );
   SESSION_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := SESSION_DEF'Access;


   Null_Session : constant Session_Ref
      := Session_Ref'(ADO.Objects.Object_Ref with null record);

   type Session_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => SESSION_DEF'Access)
   with record
       Start_Date : Ada.Calendar.Time;
       End_Date : ADO.Nullable_Time;
       Ip_Address : Ada.Strings.Unbounded.Unbounded_String;
       Stype : AWA.Users.Models.Session_Type;
       Version : Integer;
       Server_Id : Integer;
       Auth : AWA.Users.Models.Session_Ref;
       User : AWA.Users.Models.User_Ref;
   end record;

   type Session_Access is access all Session_Impl;

   overriding
   procedure Destroy (Object : access Session_Impl);

   overriding
   procedure Find (Object  : in out Session_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Session_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Session_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Session_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Session_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Session_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Session_Ref'Class;
                        Impl   : out Session_Access);
end AWA.Users.Models;
