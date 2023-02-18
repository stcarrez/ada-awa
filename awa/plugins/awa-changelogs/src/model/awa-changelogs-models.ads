-----------------------------------------------------------------------
--  AWA.Changelogs.Models -- AWA.Changelogs.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.4.0
-----------------------------------------------------------------------
--  Copyright (C) 2023 Stephane Carrez
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
with Util.Beans.Basic.Lists;
with AWA.Users.Models;
pragma Warnings (On);
package AWA.Changelogs.Models is

   pragma Style_Checks ("-mrIu");

   type Changelog_Ref is new ADO.Objects.Object_Ref with null record;

   --  Create an object key for Changelog.
   function Changelog_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Changelog from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Changelog_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Changelog : constant Changelog_Ref;
   function "=" (Left, Right : Changelog_Ref'Class) return Boolean;

   --  Set the changelog identifier.
   procedure Set_Id (Object : in out Changelog_Ref;
                     Value  : in ADO.Identifier);

   --  Get the changelog identifier.
   function Get_Id (Object : in Changelog_Ref)
                 return ADO.Identifier;

   --  Set the changelog date.
   procedure Set_Date (Object : in out Changelog_Ref;
                       Value  : in Ada.Calendar.Time);

   --  Get the changelog date.
   function Get_Date (Object : in Changelog_Ref)
                 return Ada.Calendar.Time;

   --  Set the changelog text.
   procedure Set_Text (Object : in out Changelog_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Text (Object : in out Changelog_Ref;
                       Value : in String);

   --  Get the changelog text.
   function Get_Text (Object : in Changelog_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Text (Object : in Changelog_Ref)
                 return String;

   --  Set the optional entity to which the changelog is associated.
   procedure Set_For_Entity_Id (Object : in out Changelog_Ref;
                                Value  : in ADO.Identifier);

   --  Get the optional entity to which the changelog is associated.
   function Get_For_Entity_Id (Object : in Changelog_Ref)
                 return ADO.Identifier;

   --
   procedure Set_User (Object : in out Changelog_Ref;
                       Value  : in AWA.Users.Models.User_Ref'Class);

   --
   function Get_User (Object : in Changelog_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --
   procedure Set_Entity_Type (Object : in out Changelog_Ref;
                              Value  : in ADO.Entity_Type);

   --
   function Get_Entity_Type (Object : in Changelog_Ref)
                 return ADO.Entity_Type;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Changelog_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Changelog_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Changelog_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Changelog_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Changelog_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Changelog_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   CHANGELOG_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Changelog_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Changelog_Ref;
                   Into   : in out Changelog_Ref);




private
   CHANGELOG_NAME : aliased constant String := "awa_changelog";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "date";
   COL_2_1_NAME : aliased constant String := "text";
   COL_3_1_NAME : aliased constant String := "for_entity_id";
   COL_4_1_NAME : aliased constant String := "user_id";
   COL_5_1_NAME : aliased constant String := "entity_type";

   CHANGELOG_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 6,
      Table   => CHANGELOG_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access,
         6 => COL_5_1_NAME'Access)
     );
   CHANGELOG_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := CHANGELOG_DEF'Access;


   Null_Changelog : constant Changelog_Ref
      := Changelog_Ref'(ADO.Objects.Object_Ref with null record);

   type Changelog_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => CHANGELOG_DEF'Access)
   with record
       Date : Ada.Calendar.Time;
       Text : Ada.Strings.Unbounded.Unbounded_String;
       For_Entity_Id : ADO.Identifier;
       User : AWA.Users.Models.User_Ref;
       Entity_Type : ADO.Entity_Type;
   end record;

   type Changelog_Access is access all Changelog_Impl;

   overriding
   procedure Destroy (Object : access Changelog_Impl);

   overriding
   procedure Find (Object  : in out Changelog_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Changelog_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Changelog_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Changelog_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Changelog_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Changelog_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Changelog_Ref'Class;
                        Impl   : out Changelog_Access);
end AWA.Changelogs.Models;
