-----------------------------------------------------------------------
--  AWA.Workspaces.Models -- AWA.Workspaces.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.4.0
-----------------------------------------------------------------------
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
pragma Warnings (Off);
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with ADO.Queries;
with ADO.Queries.Loaders;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Basic.Lists;
with AWA.Events;
with AWA.Users.Models;
with Util.Beans.Methods;
pragma Warnings (On);
package AWA.Workspaces.Models is

   pragma Style_Checks ("-mrIu");

   type Workspace_Ref is new ADO.Objects.Object_Ref with null record;

   type Workspace_Member_Ref is new ADO.Objects.Object_Ref with null record;

   type Invitation_Ref is new ADO.Objects.Object_Ref with null record;

   type Workspace_Feature_Ref is new ADO.Objects.Object_Ref with null record;

   --  --------------------
   --  The workspace controls the features available in the application
   --  for a set of users: the workspace members.  A user could create
   --  several workspaces and be part of several workspaces that other
   --  users have created.
   --  --------------------
   --  Create an object key for Workspace.
   function Workspace_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Workspace from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Workspace_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Workspace : constant Workspace_Ref;
   function "=" (Left, Right : Workspace_Ref'Class) return Boolean;

   --  Set the workspace identifier
   procedure Set_Id (Object : in out Workspace_Ref;
                     Value  : in ADO.Identifier);

   --  Get the workspace identifier
   function Get_Id (Object : in Workspace_Ref)
                 return ADO.Identifier;
   --
   function Get_Version (Object : in Workspace_Ref)
                 return Integer;

   --
   procedure Set_Create_Date (Object : in out Workspace_Ref;
                              Value  : in Ada.Calendar.Time);

   --
   function Get_Create_Date (Object : in Workspace_Ref)
                 return Ada.Calendar.Time;

   --
   procedure Set_Owner (Object : in out Workspace_Ref;
                        Value  : in AWA.Users.Models.User_Ref'Class);

   --
   function Get_Owner (Object : in Workspace_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Workspace_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Workspace_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Reload from the database the same object if it was modified.
   --  Returns True in `Updated` if the object was reloaded.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Reload (Object  : in out Workspace_Ref;
                     Session : in out ADO.Sessions.Session'Class;
                     Updated : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Workspace_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Workspace_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Workspace_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Workspace_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   WORKSPACE_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Workspace_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Workspace_Ref;
                   Into   : in out Workspace_Ref);

   --  --------------------
   --  The workspace member indicates the users who
   --  are part of the workspace. The join_date is NULL when
   --  a user was invited but has not accepted the invitation.
   --  --------------------
   --  Create an object key for Workspace_Member.
   function Workspace_Member_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Workspace_Member from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Workspace_Member_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Workspace_Member : constant Workspace_Member_Ref;
   function "=" (Left, Right : Workspace_Member_Ref'Class) return Boolean;

   --
   procedure Set_Id (Object : in out Workspace_Member_Ref;
                     Value  : in ADO.Identifier);

   --
   function Get_Id (Object : in Workspace_Member_Ref)
                 return ADO.Identifier;

   --  Set the date when the user has joined the workspace.
   procedure Set_Join_Date (Object : in out Workspace_Member_Ref;
                            Value  : in ADO.Nullable_Time);

   --  Get the date when the user has joined the workspace.
   function Get_Join_Date (Object : in Workspace_Member_Ref)
                 return ADO.Nullable_Time;

   --  Set the member role.
   procedure Set_Role (Object : in out Workspace_Member_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Role (Object : in out Workspace_Member_Ref;
                       Value : in String);

   --  Get the member role.
   function Get_Role (Object : in Workspace_Member_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Role (Object : in Workspace_Member_Ref)
                 return String;

   --
   procedure Set_Member (Object : in out Workspace_Member_Ref;
                         Value  : in AWA.Users.Models.User_Ref'Class);

   --
   function Get_Member (Object : in Workspace_Member_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --
   procedure Set_Workspace (Object : in out Workspace_Member_Ref;
                            Value  : in Workspace_Ref'Class);

   --
   function Get_Workspace (Object : in Workspace_Member_Ref)
                 return Workspace_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Workspace_Member_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Workspace_Member_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Workspace_Member_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Workspace_Member_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Workspace_Member_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Workspace_Member_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   WORKSPACE_MEMBER_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Workspace_Member_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Workspace_Member_Ref;
                   Into   : in out Workspace_Member_Ref);

   --  Create an object key for Invitation.
   function Invitation_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Invitation from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Invitation_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Invitation : constant Invitation_Ref;
   function "=" (Left, Right : Invitation_Ref'Class) return Boolean;

   --  Set the invitation identifier.
   procedure Set_Id (Object : in out Invitation_Ref;
                     Value  : in ADO.Identifier);

   --  Get the invitation identifier.
   function Get_Id (Object : in Invitation_Ref)
                 return ADO.Identifier;
   --  Get version optimistic lock.
   function Get_Version (Object : in Invitation_Ref)
                 return Integer;

   --  Set date when the invitation was created and sent.
   procedure Set_Create_Date (Object : in out Invitation_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get date when the invitation was created and sent.
   function Get_Create_Date (Object : in Invitation_Ref)
                 return Ada.Calendar.Time;

   --  Set the email address to which the invitation was sent.
   procedure Set_Email (Object : in out Invitation_Ref;
                        Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Email (Object : in out Invitation_Ref;
                        Value : in String);

   --  Get the email address to which the invitation was sent.
   function Get_Email (Object : in Invitation_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Email (Object : in Invitation_Ref)
                 return String;

   --  Set the invitation message.
   procedure Set_Message (Object : in out Invitation_Ref;
                          Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Message (Object : in out Invitation_Ref;
                          Value : in String);

   --  Get the invitation message.
   function Get_Message (Object : in Invitation_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Message (Object : in Invitation_Ref)
                 return String;

   --  Set the date when the invitation was accepted.
   procedure Set_Acceptance_Date (Object : in out Invitation_Ref;
                                  Value  : in ADO.Nullable_Time);

   --  Get the date when the invitation was accepted.
   function Get_Acceptance_Date (Object : in Invitation_Ref)
                 return ADO.Nullable_Time;

   --  Set the workspace where the user is invited.
   procedure Set_Workspace (Object : in out Invitation_Ref;
                            Value  : in Workspace_Ref'Class);

   --  Get the workspace where the user is invited.
   function Get_Workspace (Object : in Invitation_Ref)
                 return Workspace_Ref'Class;

   --
   procedure Set_Access_Key (Object : in out Invitation_Ref;
                             Value  : in AWA.Users.Models.Access_Key_Ref'Class);

   --
   function Get_Access_Key (Object : in Invitation_Ref)
                 return AWA.Users.Models.Access_Key_Ref'Class;

   --  Set the user being invited.
   procedure Set_Invitee (Object : in out Invitation_Ref;
                          Value  : in AWA.Users.Models.User_Ref'Class);

   --  Get the user being invited.
   function Get_Invitee (Object : in Invitation_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --
   procedure Set_Inviter (Object : in out Invitation_Ref;
                          Value  : in AWA.Users.Models.User_Ref'Class);

   --
   function Get_Inviter (Object : in Invitation_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --
   procedure Set_Member (Object : in out Invitation_Ref;
                         Value  : in Workspace_Member_Ref'Class);

   --
   function Get_Member (Object : in Invitation_Ref)
                 return Workspace_Member_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Invitation_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Invitation_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Reload from the database the same object if it was modified.
   --  Returns True in `Updated` if the object was reloaded.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Reload (Object  : in out Invitation_Ref;
                     Session : in out ADO.Sessions.Session'Class;
                     Updated : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Invitation_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Invitation_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Invitation_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Invitation_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   INVITATION_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Invitation_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Invitation_Ref;
                   Into   : in out Invitation_Ref);

   --  Create an object key for Workspace_Feature.
   function Workspace_Feature_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Workspace_Feature from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Workspace_Feature_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Workspace_Feature : constant Workspace_Feature_Ref;
   function "=" (Left, Right : Workspace_Feature_Ref'Class) return Boolean;

   --
   procedure Set_Id (Object : in out Workspace_Feature_Ref;
                     Value  : in ADO.Identifier);

   --
   function Get_Id (Object : in Workspace_Feature_Ref)
                 return ADO.Identifier;

   --
   procedure Set_Limit (Object : in out Workspace_Feature_Ref;
                        Value  : in Integer);

   --
   function Get_Limit (Object : in Workspace_Feature_Ref)
                 return Integer;

   --
   procedure Set_Workspace (Object : in out Workspace_Feature_Ref;
                            Value  : in Workspace_Ref'Class);

   --
   function Get_Workspace (Object : in Workspace_Feature_Ref)
                 return Workspace_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Workspace_Feature_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Workspace_Feature_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Workspace_Feature_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Workspace_Feature_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Workspace_Feature_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Workspace_Feature_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   WORKSPACE_FEATURE_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Workspace_Feature_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Workspace_Feature_Ref;
                   Into   : in out Workspace_Feature_Ref);



   Query_Member_In_Role : constant ADO.Queries.Query_Definition_Access;

   --  --------------------
   --    The Member_Info describes a member of the workspace.
   --  --------------------
   type Member_Info is
     new Util.Beans.Basic.Bean with  record

      --  the member identifier.
      Id : ADO.Identifier;

      --  the user identifier.
      User_Id : ADO.Identifier;

      --  the user name.
      Name : Ada.Strings.Unbounded.Unbounded_String;

      --  the user email address.
      Email : Ada.Strings.Unbounded.Unbounded_String;

      --  the user's role.
      Role : Ada.Strings.Unbounded.Unbounded_String;

      --  the date when the user joined the workspace.
      Join_Date : ADO.Nullable_Time;

      --  the date when the invitation was sent to the user.
      Invite_Date : ADO.Nullable_Time;
   end record;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Member_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Member_Info;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);


   package Member_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Member_Info);
   package Member_Info_Vectors renames Member_Info_Beans.Vectors;
   subtype Member_Info_List_Bean is Member_Info_Beans.List_Bean;

   type Member_Info_List_Bean_Access is access all Member_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Member_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Member_Info_Vector is Member_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Member_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Workspace_Member_List : constant ADO.Queries.Query_Definition_Access;


   --  --------------------
   --    Operation to load the invitation.
   --  --------------------
   type Invitation_Bean is abstract new AWA.Workspaces.Models.Invitation_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with  record
      Key : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Invitation_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Invitation_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Invitation_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Load (Bean : in out Invitation_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Accept_Invitation (Bean  : in out Invitation_Bean;
                                Event : in AWA.Events.Module_Event'Class) is abstract;

   procedure Send (Bean : in out Invitation_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   --  --------------------
   --    load the list of members.
   --  --------------------
   type Member_List_Bean is abstract limited
     new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with  record

      --  the number of members per page.
      Page_Size : Integer;

      --  the number of pages.
      Count : Integer;

      --  the current page number.
      Page : Integer;
   end record;

   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Member_List_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Member_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Member_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Load (Bean : in out Member_List_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   --  --------------------
   --    load the member information.
   --  --------------------
   type Member_Bean is abstract new AWA.Workspaces.Models.Workspace_Member_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with null record;


   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Member_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;


   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Member_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Load (Bean : in out Member_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Delete (Bean : in out Member_Bean;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;


private
   WORKSPACE_NAME : aliased constant String := "awa_workspace";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "version";
   COL_2_1_NAME : aliased constant String := "create_date";
   COL_3_1_NAME : aliased constant String := "owner_id";

   WORKSPACE_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 4,
      Table   => WORKSPACE_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access)
     );
   WORKSPACE_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := WORKSPACE_DEF'Access;


   Null_Workspace : constant Workspace_Ref
      := Workspace_Ref'(ADO.Objects.Object_Ref with null record);

   type Workspace_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => WORKSPACE_DEF'Access)
   with record
       Version : Integer;
       Create_Date : Ada.Calendar.Time;
       Owner : AWA.Users.Models.User_Ref;
   end record;

   type Workspace_Access is access all Workspace_Impl;

   overriding
   procedure Destroy (Object : access Workspace_Impl);

   overriding
   procedure Find (Object  : in out Workspace_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Workspace_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Workspace_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Workspace_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Workspace_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Workspace_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Workspace_Ref'Class;
                        Impl   : out Workspace_Access);
   WORKSPACE_MEMBER_NAME : aliased constant String := "awa_workspace_member";
   COL_0_2_NAME : aliased constant String := "id";
   COL_1_2_NAME : aliased constant String := "join_date";
   COL_2_2_NAME : aliased constant String := "role";
   COL_3_2_NAME : aliased constant String := "member_id";
   COL_4_2_NAME : aliased constant String := "workspace_id";

   WORKSPACE_MEMBER_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 5,
      Table   => WORKSPACE_MEMBER_NAME'Access,
      Members => (
         1 => COL_0_2_NAME'Access,
         2 => COL_1_2_NAME'Access,
         3 => COL_2_2_NAME'Access,
         4 => COL_3_2_NAME'Access,
         5 => COL_4_2_NAME'Access)
     );
   WORKSPACE_MEMBER_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := WORKSPACE_MEMBER_DEF'Access;


   Null_Workspace_Member : constant Workspace_Member_Ref
      := Workspace_Member_Ref'(ADO.Objects.Object_Ref with null record);

   type Workspace_Member_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => WORKSPACE_MEMBER_DEF'Access)
   with record
       Join_Date : ADO.Nullable_Time;
       Role : Ada.Strings.Unbounded.Unbounded_String;
       Member : AWA.Users.Models.User_Ref;
       Workspace : Workspace_Ref;
   end record;

   type Workspace_Member_Access is access all Workspace_Member_Impl;

   overriding
   procedure Destroy (Object : access Workspace_Member_Impl);

   overriding
   procedure Find (Object  : in out Workspace_Member_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Workspace_Member_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Workspace_Member_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Workspace_Member_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Workspace_Member_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Workspace_Member_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Workspace_Member_Ref'Class;
                        Impl   : out Workspace_Member_Access);
   INVITATION_NAME : aliased constant String := "awa_invitation";
   COL_0_3_NAME : aliased constant String := "id";
   COL_1_3_NAME : aliased constant String := "version";
   COL_2_3_NAME : aliased constant String := "create_date";
   COL_3_3_NAME : aliased constant String := "email";
   COL_4_3_NAME : aliased constant String := "message";
   COL_5_3_NAME : aliased constant String := "acceptance_date";
   COL_6_3_NAME : aliased constant String := "workspace_id";
   COL_7_3_NAME : aliased constant String := "access_key_id";
   COL_8_3_NAME : aliased constant String := "invitee_id";
   COL_9_3_NAME : aliased constant String := "inviter_id";
   COL_10_3_NAME : aliased constant String := "member_id";

   INVITATION_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 11,
      Table   => INVITATION_NAME'Access,
      Members => (
         1 => COL_0_3_NAME'Access,
         2 => COL_1_3_NAME'Access,
         3 => COL_2_3_NAME'Access,
         4 => COL_3_3_NAME'Access,
         5 => COL_4_3_NAME'Access,
         6 => COL_5_3_NAME'Access,
         7 => COL_6_3_NAME'Access,
         8 => COL_7_3_NAME'Access,
         9 => COL_8_3_NAME'Access,
         10 => COL_9_3_NAME'Access,
         11 => COL_10_3_NAME'Access)
     );
   INVITATION_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := INVITATION_DEF'Access;


   Null_Invitation : constant Invitation_Ref
      := Invitation_Ref'(ADO.Objects.Object_Ref with null record);

   type Invitation_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => INVITATION_DEF'Access)
   with record
       Version : Integer;
       Create_Date : Ada.Calendar.Time;
       Email : Ada.Strings.Unbounded.Unbounded_String;
       Message : Ada.Strings.Unbounded.Unbounded_String;
       Acceptance_Date : ADO.Nullable_Time;
       Workspace : Workspace_Ref;
       Access_Key : AWA.Users.Models.Access_Key_Ref;
       Invitee : AWA.Users.Models.User_Ref;
       Inviter : AWA.Users.Models.User_Ref;
       Member : Workspace_Member_Ref;
   end record;

   type Invitation_Access is access all Invitation_Impl;

   overriding
   procedure Destroy (Object : access Invitation_Impl);

   overriding
   procedure Find (Object  : in out Invitation_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Invitation_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Invitation_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Invitation_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Invitation_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Invitation_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Invitation_Ref'Class;
                        Impl   : out Invitation_Access);
   WORKSPACE_FEATURE_NAME : aliased constant String := "awa_workspace_feature";
   COL_0_4_NAME : aliased constant String := "id";
   COL_1_4_NAME : aliased constant String := "limit";
   COL_2_4_NAME : aliased constant String := "workspace_id";

   WORKSPACE_FEATURE_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 3,
      Table   => WORKSPACE_FEATURE_NAME'Access,
      Members => (
         1 => COL_0_4_NAME'Access,
         2 => COL_1_4_NAME'Access,
         3 => COL_2_4_NAME'Access)
     );
   WORKSPACE_FEATURE_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := WORKSPACE_FEATURE_DEF'Access;


   Null_Workspace_Feature : constant Workspace_Feature_Ref
      := Workspace_Feature_Ref'(ADO.Objects.Object_Ref with null record);

   type Workspace_Feature_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => WORKSPACE_FEATURE_DEF'Access)
   with record
       Limit : Integer;
       Workspace : Workspace_Ref;
   end record;

   type Workspace_Feature_Access is access all Workspace_Feature_Impl;

   overriding
   procedure Destroy (Object : access Workspace_Feature_Impl);

   overriding
   procedure Find (Object  : in out Workspace_Feature_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Workspace_Feature_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Workspace_Feature_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Workspace_Feature_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Workspace_Feature_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Workspace_Feature_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Workspace_Feature_Ref'Class;
                        Impl   : out Workspace_Feature_Access);

   package File_1 is
      new ADO.Queries.Loaders.File (Path => "workspace-permissions.xml",
                                    Sha1 => "9B2B599473F75F92CB5AB5045675E4CCEF926543");

   package Def_Member_In_Role is
      new ADO.Queries.Loaders.Query (Name => "member-in-role",
                                     File => File_1.File'Access);
   Query_Member_In_Role : constant ADO.Queries.Query_Definition_Access
   := Def_Member_In_Role.Query'Access;

   package File_2 is
      new ADO.Queries.Loaders.File (Path => "member-list.xml",
                                    Sha1 => "D92F1CB6DBE47F7A72E83CD4AE8E39F2048D0728");

   package Def_Memberinfo_Workspace_Member_List is
      new ADO.Queries.Loaders.Query (Name => "workspace-member-list",
                                     File => File_2.File'Access);
   Query_Workspace_Member_List : constant ADO.Queries.Query_Definition_Access
   := Def_Memberinfo_Workspace_Member_List.Query'Access;
end AWA.Workspaces.Models;
