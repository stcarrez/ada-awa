-----------------------------------------------------------------------
--  AWA.Permissions.Models -- AWA.Permissions.Models
-----------------------------------------------------------------------
--  File generated by ada-gen DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://ada-gen.googlecode.com/svn/trunk Revision 1095
-----------------------------------------------------------------------
--  Copyright (C) 2015 Stephane Carrez
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
with ADO.Queries;
with ADO.Queries.Loaders;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Basic.Lists;
pragma Warnings (On);
package AWA.Permissions.Models is

   pragma Style_Checks ("-mr");

   type ACL_Ref is new ADO.Objects.Object_Ref with null record;

   --  --------------------
   --  The ACL table records permissions which are granted for a user to access a given database entity.
   --  --------------------
   --  Create an object key for ACL.
   function ACL_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for ACL from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function ACL_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_ACL : constant ACL_Ref;
   function "=" (Left, Right : ACL_Ref'Class) return Boolean;

   --  Set the ACL identifier
   procedure Set_Id (Object : in out Acl_Ref;
                     Value  : in ADO.Identifier);

   --  Get the ACL identifier
   function Get_Id (Object : in Acl_Ref)
                 return ADO.Identifier;

   --  Set the entity identifier to which the ACL applies
   procedure Set_Entity_Id (Object : in out Acl_Ref;
                            Value  : in ADO.Identifier);

   --  Get the entity identifier to which the ACL applies
   function Get_Entity_Id (Object : in Acl_Ref)
                 return ADO.Identifier;

   --  Set the writeable flag
   procedure Set_Writeable (Object : in out Acl_Ref;
                            Value  : in Boolean);

   --  Get the writeable flag
   function Get_Writeable (Object : in Acl_Ref)
                 return Boolean;

   --
   procedure Set_User_Id (Object : in out Acl_Ref;
                          Value  : in ADO.Identifier);

   --
   function Get_User_Id (Object : in Acl_Ref)
                 return ADO.Identifier;

   --  Set the entity type concerned by the ACL.
   procedure Set_Entity_Type (Object : in out Acl_Ref;
                              Value  : in ADO.Entity_Type);

   --  Get the entity type concerned by the ACL.
   function Get_Entity_Type (Object : in Acl_Ref)
                 return ADO.Entity_Type;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Acl_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Acl_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Acl_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Acl_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Acl_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Acl_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   ACL_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Acl_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Acl_Ref;
                   Into   : in out Acl_Ref);



   Query_Check_Entity_Permission : constant ADO.Queries.Query_Definition_Access;

   Query_Remove_Permission : constant ADO.Queries.Query_Definition_Access;

   Query_Remove_Entity_Permission : constant ADO.Queries.Query_Definition_Access;

   Query_Remove_User_Permission : constant ADO.Queries.Query_Definition_Access;



private
   ACL_NAME : aliased constant String := "awa_acl";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "entity_id";
   COL_2_1_NAME : aliased constant String := "writeable";
   COL_3_1_NAME : aliased constant String := "user_id";
   COL_4_1_NAME : aliased constant String := "entity_type";

   ACL_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count => 5,
      Table => ACL_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access
)
     );
   ACL_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := ACL_DEF'Access;

   Null_ACL : constant ACL_Ref
      := ACL_Ref'(ADO.Objects.Object_Ref with others => <>);

   type Acl_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => ACL_DEF'Access)
   with record
       Entity_Id : ADO.Identifier;
       Writeable : Boolean;
       User_Id : ADO.Identifier;
       Entity_Type : ADO.Entity_Type;
   end record;

   type Acl_Access is access all Acl_Impl;

   overriding
   procedure Destroy (Object : access Acl_Impl);

   overriding
   procedure Find (Object  : in out Acl_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Acl_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Acl_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Acl_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Acl_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Acl_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Acl_Ref'Class;
                        Impl   : out Acl_Access);

   package File_1 is
      new ADO.Queries.Loaders.File (Path => "permissions.xml",
                                    Sha1 => "9B2B599473F75F92CB5AB5045675E4CCEF926543");

   package Def_Check_Entity_Permission is
      new ADO.Queries.Loaders.Query (Name => "check-entity-permission",
                                     File => File_1.File'Access);
   Query_Check_Entity_Permission : constant ADO.Queries.Query_Definition_Access
   := Def_Check_Entity_Permission.Query'Access;

   package Def_Remove_Permission is
      new ADO.Queries.Loaders.Query (Name => "remove-permission",
                                     File => File_1.File'Access);
   Query_Remove_Permission : constant ADO.Queries.Query_Definition_Access
   := Def_Remove_Permission.Query'Access;

   package Def_Remove_Entity_Permission is
      new ADO.Queries.Loaders.Query (Name => "remove-entity-permission",
                                     File => File_1.File'Access);
   Query_Remove_Entity_Permission : constant ADO.Queries.Query_Definition_Access
   := Def_Remove_Entity_Permission.Query'Access;

   package Def_Remove_User_Permission is
      new ADO.Queries.Loaders.Query (Name => "remove-user-permission",
                                     File => File_1.File'Access);
   Query_Remove_User_Permission : constant ADO.Queries.Query_Definition_Access
   := Def_Remove_User_Permission.Query'Access;
end AWA.Permissions.Models;
