-----------------------------------------------------------------------
--  AWA.Audits.Models -- AWA.Audits.Models
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
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Basic.Lists;
with AWA.Users.Models;
pragma Warnings (On);
package AWA.Audits.Models is

   pragma Style_Checks ("-mrIu");

   type Audit_Ref is new ADO.Objects.Object_Ref with null record;

   type Audit_Field_Ref is new ADO.Objects.Object_Ref with null record;

   --  --------------------
   --  The Audit table records the changes made on database on behalf of a user.
   --  The record indicates the database table and row, the field being updated,
   --  the old and new value. The old and new values are converted to a string
   --  and they truncated if necessary to 256 characters.
   --  --------------------
   --  Create an object key for Audit.
   function Audit_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Audit from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Audit_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Audit : constant Audit_Ref;
   function "=" (Left, Right : Audit_Ref'Class) return Boolean;

   --  Set the audit identifier
   procedure Set_Id (Object : in out Audit_Ref;
                     Value  : in ADO.Identifier);

   --  Get the audit identifier
   function Get_Id (Object : in Audit_Ref)
                 return ADO.Identifier;

   --  Set the date when the field was modified.
   procedure Set_Date (Object : in out Audit_Ref;
                       Value  : in Ada.Calendar.Time);

   --  Get the date when the field was modified.
   function Get_Date (Object : in Audit_Ref)
                 return Ada.Calendar.Time;

   --  Set the old field value.
   procedure Set_Old_Value (Object : in out Audit_Ref;
                            Value  : in ADO.Nullable_String);
   procedure Set_Old_Value (Object : in out Audit_Ref;
                            Value : in String);

   --  Get the old field value.
   function Get_Old_Value (Object : in Audit_Ref)
                 return ADO.Nullable_String;
   function Get_Old_Value (Object : in Audit_Ref)
                 return String;

   --  Set the new field value.
   procedure Set_New_Value (Object : in out Audit_Ref;
                            Value  : in ADO.Nullable_String);
   procedure Set_New_Value (Object : in out Audit_Ref;
                            Value : in String);

   --  Get the new field value.
   function Get_New_Value (Object : in Audit_Ref)
                 return ADO.Nullable_String;
   function Get_New_Value (Object : in Audit_Ref)
                 return String;

   --  Set the database entity identifier to which the audit is associated.
   procedure Set_Entity_Id (Object : in out Audit_Ref;
                            Value  : in ADO.Identifier);

   --  Get the database entity identifier to which the audit is associated.
   function Get_Entity_Id (Object : in Audit_Ref)
                 return ADO.Identifier;

   --
   procedure Set_Field (Object : in out Audit_Ref;
                        Value  : in Integer);

   --
   function Get_Field (Object : in Audit_Ref)
                 return Integer;

   --  Set the user session under which the field was modified.
   procedure Set_Session (Object : in out Audit_Ref;
                          Value  : in AWA.Users.Models.Session_Ref'Class);

   --  Get the user session under which the field was modified.
   function Get_Session (Object : in Audit_Ref)
                 return AWA.Users.Models.Session_Ref'Class;

   --  Set the entity type.
   procedure Set_Entity_Type (Object : in out Audit_Ref;
                              Value  : in ADO.Entity_Type);

   --  Get the entity type.
   function Get_Entity_Type (Object : in Audit_Ref)
                 return ADO.Entity_Type;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Audit_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Audit_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   AUDIT_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Audit_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Audit_Ref;
                   Into   : in out Audit_Ref);

   package Audit_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Audit_Ref,
                                  "="          => "=");
   subtype Audit_Vector is Audit_Vectors.Vector;

   procedure List (Object  : in out Audit_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class);
   --  --------------------
   --  The Audit_Field table describes
   --  the database field being updated.
   --  --------------------
   --  Create an object key for Audit_Field.
   function Audit_Field_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Audit_Field from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Audit_Field_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Audit_Field : constant Audit_Field_Ref;
   function "=" (Left, Right : Audit_Field_Ref'Class) return Boolean;

   --  Set the audit field identifier.
   procedure Set_Id (Object : in out Audit_Field_Ref;
                     Value  : in Integer);

   --  Get the audit field identifier.
   function Get_Id (Object : in Audit_Field_Ref)
                 return Integer;

   --  Set the audit field name.
   procedure Set_Name (Object : in out Audit_Field_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out Audit_Field_Ref;
                       Value : in String);

   --  Get the audit field name.
   function Get_Name (Object : in Audit_Field_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in Audit_Field_Ref)
                 return String;

   --  Set the entity type
   procedure Set_Entity_Type (Object : in out Audit_Field_Ref;
                              Value  : in ADO.Entity_Type);

   --  Get the entity type
   function Get_Entity_Type (Object : in Audit_Field_Ref)
                 return ADO.Entity_Type;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Audit_Field_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in Integer);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Audit_Field_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in Integer;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Audit_Field_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Audit_Field_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Audit_Field_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Audit_Field_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   AUDIT_FIELD_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Audit_Field_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Audit_Field_Ref;
                   Into   : in out Audit_Field_Ref);




private
   AUDIT_NAME : aliased constant String := "awa_audit";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "date";
   COL_2_1_NAME : aliased constant String := "old_value";
   COL_3_1_NAME : aliased constant String := "new_value";
   COL_4_1_NAME : aliased constant String := "entity_id";
   COL_5_1_NAME : aliased constant String := "field";
   COL_6_1_NAME : aliased constant String := "session_id";
   COL_7_1_NAME : aliased constant String := "entity_type";

   AUDIT_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 8,
      Table   => AUDIT_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access,
         6 => COL_5_1_NAME'Access,
         7 => COL_6_1_NAME'Access,
         8 => COL_7_1_NAME'Access)
     );
   AUDIT_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := AUDIT_DEF'Access;


   Null_Audit : constant Audit_Ref
      := Audit_Ref'(ADO.Objects.Object_Ref with null record);

   type Audit_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => AUDIT_DEF'Access)
   with record
       Date : Ada.Calendar.Time;
       Old_Value : ADO.Nullable_String;
       New_Value : ADO.Nullable_String;
       Entity_Id : ADO.Identifier;
       Field : Integer;
       Session : AWA.Users.Models.Session_Ref;
       Entity_Type : ADO.Entity_Type;
   end record;

   type Audit_Access is access all Audit_Impl;

   overriding
   procedure Destroy (Object : access Audit_Impl);

   overriding
   procedure Find (Object  : in out Audit_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Audit_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Audit_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Audit_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Audit_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Audit_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Audit_Ref'Class;
                        Impl   : out Audit_Access);
   AUDIT_FIELD_NAME : aliased constant String := "awa_audit_field";
   COL_0_2_NAME : aliased constant String := "id";
   COL_1_2_NAME : aliased constant String := "name";
   COL_2_2_NAME : aliased constant String := "entity_type";

   AUDIT_FIELD_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 3,
      Table   => AUDIT_FIELD_NAME'Access,
      Members => (
         1 => COL_0_2_NAME'Access,
         2 => COL_1_2_NAME'Access,
         3 => COL_2_2_NAME'Access)
     );
   AUDIT_FIELD_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := AUDIT_FIELD_DEF'Access;


   Null_Audit_Field : constant Audit_Field_Ref
      := Audit_Field_Ref'(ADO.Objects.Object_Ref with null record);

   type Audit_Field_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_STRING,
                                     Of_Class => AUDIT_FIELD_DEF'Access)
   with record
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Entity_Type : ADO.Entity_Type;
   end record;

   type Audit_Field_Access is access all Audit_Field_Impl;

   overriding
   procedure Destroy (Object : access Audit_Field_Impl);

   overriding
   procedure Find (Object  : in out Audit_Field_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Audit_Field_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Audit_Field_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Audit_Field_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Audit_Field_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Audit_Field_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Audit_Field_Ref'Class;
                        Impl   : out Audit_Field_Access);
end AWA.Audits.Models;
