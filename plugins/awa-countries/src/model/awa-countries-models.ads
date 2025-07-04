-----------------------------------------------------------------------
--  AWA.Countries.Models -- AWA.Countries.Models
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
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Basic.Lists;
pragma Warnings (On);
package AWA.Countries.Models is

   pragma Style_Checks ("-mrIu");

   type Country_Ref is new ADO.Objects.Object_Ref with null record;

   type City_Ref is new ADO.Objects.Object_Ref with null record;

   type Country_Neighbor_Ref is new ADO.Objects.Object_Ref with null record;

   type Region_Ref is new ADO.Objects.Object_Ref with null record;

   --  --------------------
   --  The country model is a system data model for the application.
   --  In theory, it never changes.
   --  --------------------
   --  Create an object key for Country.
   function Country_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Country from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Country_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Country : constant Country_Ref;
   function "=" (Left, Right : Country_Ref'Class) return Boolean;

   --  Set the country identifier
   procedure Set_Id (Object : in out Country_Ref;
                     Value  : in ADO.Identifier);

   --  Get the country identifier
   function Get_Id (Object : in Country_Ref)
                 return ADO.Identifier;

   --  Set the country name
   procedure Set_Name (Object : in out Country_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out Country_Ref;
                       Value : in String);

   --  Get the country name
   function Get_Name (Object : in Country_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in Country_Ref)
                 return String;

   --  Set the continent name
   procedure Set_Continent (Object : in out Country_Ref;
                            Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Continent (Object : in out Country_Ref;
                            Value : in String);

   --  Get the continent name
   function Get_Continent (Object : in Country_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Continent (Object : in Country_Ref)
                 return String;

   --  Set the currency used in the country
   procedure Set_Currency (Object : in out Country_Ref;
                           Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Currency (Object : in out Country_Ref;
                           Value : in String);

   --  Get the currency used in the country
   function Get_Currency (Object : in Country_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Currency (Object : in Country_Ref)
                 return String;

   --  Set the country ISO code
   procedure Set_Iso_Code (Object : in out Country_Ref;
                           Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Iso_Code (Object : in out Country_Ref;
                           Value : in String);

   --  Get the country ISO code
   function Get_Iso_Code (Object : in Country_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Iso_Code (Object : in Country_Ref)
                 return String;

   --  Set the country geoname id
   procedure Set_Geonameid (Object : in out Country_Ref;
                            Value  : in Integer);

   --  Get the country geoname id
   function Get_Geonameid (Object : in Country_Ref)
                 return Integer;

   --  Set the country main language
   procedure Set_Languages (Object : in out Country_Ref;
                            Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Languages (Object : in out Country_Ref;
                            Value : in String);

   --  Get the country main language
   function Get_Languages (Object : in Country_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Languages (Object : in Country_Ref)
                 return String;

   --  Set the TLD associated with this country
   procedure Set_Tld (Object : in out Country_Ref;
                      Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Tld (Object : in out Country_Ref;
                      Value : in String);

   --  Get the TLD associated with this country
   function Get_Tld (Object : in Country_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Tld (Object : in Country_Ref)
                 return String;

   --  Set the currency code
   procedure Set_Currency_Code (Object : in out Country_Ref;
                                Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Currency_Code (Object : in out Country_Ref;
                                Value : in String);

   --  Get the currency code
   function Get_Currency_Code (Object : in Country_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Currency_Code (Object : in Country_Ref)
                 return String;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Country_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Country_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Country_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Country_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Country_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Country_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   COUNTRY_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Country_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Country_Ref;
                   Into   : in out Country_Ref);

   --  Create an object key for City.
   function City_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for City from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function City_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_City : constant City_Ref;
   function "=" (Left, Right : City_Ref'Class) return Boolean;

   --  Set the city identifier
   procedure Set_Id (Object : in out City_Ref;
                     Value  : in ADO.Identifier);

   --  Get the city identifier
   function Get_Id (Object : in City_Ref)
                 return ADO.Identifier;

   --  Set the city name
   procedure Set_Name (Object : in out City_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out City_Ref;
                       Value : in String);

   --  Get the city name
   function Get_Name (Object : in City_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in City_Ref)
                 return String;

   --  Set the city ZIP code
   procedure Set_Zip_Code (Object : in out City_Ref;
                           Value  : in Integer);

   --  Get the city ZIP code
   function Get_Zip_Code (Object : in City_Ref)
                 return Integer;

   --  Set the city latitude
   procedure Set_Latitude (Object : in out City_Ref;
                           Value  : in Integer);

   --  Get the city latitude
   function Get_Latitude (Object : in City_Ref)
                 return Integer;

   --  Set the city longitude
   procedure Set_Longitude (Object : in out City_Ref;
                            Value  : in Integer);

   --  Get the city longitude
   function Get_Longitude (Object : in City_Ref)
                 return Integer;

   --  Set the region that this city belongs to
   procedure Set_Region (Object : in out City_Ref;
                         Value  : in Region_Ref'Class);

   --  Get the region that this city belongs to
   function Get_Region (Object : in City_Ref)
                 return Region_Ref'Class;

   --  Set the country that this city belongs to
   procedure Set_Country (Object : in out City_Ref;
                          Value  : in Country_Ref'Class);

   --  Get the country that this city belongs to
   function Get_Country (Object : in City_Ref)
                 return Country_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out City_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out City_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out City_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out City_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out City_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in City_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   CITY_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out City_Ref);

   --  Copy of the object.
   procedure Copy (Object : in City_Ref;
                   Into   : in out City_Ref);

   --  --------------------
   --  The country neighbor defines what countries
   --  are neigbors with each other
   --  --------------------
   --  Create an object key for Country_Neighbor.
   function Country_Neighbor_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Country_Neighbor from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Country_Neighbor_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Country_Neighbor : constant Country_Neighbor_Ref;
   function "=" (Left, Right : Country_Neighbor_Ref'Class) return Boolean;

   --
   procedure Set_Id (Object : in out Country_Neighbor_Ref;
                     Value  : in ADO.Identifier);

   --
   function Get_Id (Object : in Country_Neighbor_Ref)
                 return ADO.Identifier;

   --
   procedure Set_Neighbor_Of (Object : in out Country_Neighbor_Ref;
                              Value  : in Country_Ref'Class);

   --
   function Get_Neighbor_Of (Object : in Country_Neighbor_Ref)
                 return Country_Ref'Class;

   --
   procedure Set_Neighbor (Object : in out Country_Neighbor_Ref;
                           Value  : in Country_Ref'Class);

   --
   function Get_Neighbor (Object : in Country_Neighbor_Ref)
                 return Country_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Country_Neighbor_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Country_Neighbor_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Country_Neighbor_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Country_Neighbor_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Country_Neighbor_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Country_Neighbor_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   COUNTRY_NEIGHBOR_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Country_Neighbor_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Country_Neighbor_Ref;
                   Into   : in out Country_Neighbor_Ref);

   --  --------------------
   --  Region defines an area within a country.
   --  --------------------
   --  Create an object key for Region.
   function Region_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Region from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Region_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Region : constant Region_Ref;
   function "=" (Left, Right : Region_Ref'Class) return Boolean;

   --  Set the region identifier
   procedure Set_Id (Object : in out Region_Ref;
                     Value  : in ADO.Identifier);

   --  Get the region identifier
   function Get_Id (Object : in Region_Ref)
                 return ADO.Identifier;

   --  Set the region name
   procedure Set_Name (Object : in out Region_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out Region_Ref;
                       Value : in String);

   --  Get the region name
   function Get_Name (Object : in Region_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in Region_Ref)
                 return String;

   --  Set the region geonameid
   procedure Set_Geonameid (Object : in out Region_Ref;
                            Value  : in Integer);

   --  Get the region geonameid
   function Get_Geonameid (Object : in Region_Ref)
                 return Integer;

   --  Set the country that this region belongs to
   procedure Set_Country (Object : in out Region_Ref;
                          Value  : in Country_Ref'Class);

   --  Get the country that this region belongs to
   function Get_Country (Object : in Region_Ref)
                 return Country_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Region_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Region_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Region_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Region_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Region_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Region_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   REGION_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Region_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Region_Ref;
                   Into   : in out Region_Ref);




private
   COUNTRY_NAME : aliased constant String := "awa_country";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "name";
   COL_2_1_NAME : aliased constant String := "continent";
   COL_3_1_NAME : aliased constant String := "currency";
   COL_4_1_NAME : aliased constant String := "iso_code";
   COL_5_1_NAME : aliased constant String := "geonameid";
   COL_6_1_NAME : aliased constant String := "languages";
   COL_7_1_NAME : aliased constant String := "tld";
   COL_8_1_NAME : aliased constant String := "currency_code";

   COUNTRY_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 9,
      Table   => COUNTRY_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access,
         6 => COL_5_1_NAME'Access,
         7 => COL_6_1_NAME'Access,
         8 => COL_7_1_NAME'Access,
         9 => COL_8_1_NAME'Access)
     );
   COUNTRY_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := COUNTRY_DEF'Access;


   Null_Country : constant Country_Ref
      := Country_Ref'(ADO.Objects.Object_Ref with null record);

   type Country_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => COUNTRY_DEF'Access)
   with record
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Continent : Ada.Strings.Unbounded.Unbounded_String;
       Currency : Ada.Strings.Unbounded.Unbounded_String;
       Iso_Code : Ada.Strings.Unbounded.Unbounded_String;
       Geonameid : Integer;
       Languages : Ada.Strings.Unbounded.Unbounded_String;
       Tld : Ada.Strings.Unbounded.Unbounded_String;
       Currency_Code : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Country_Access is access all Country_Impl;

   overriding
   procedure Destroy (Object : access Country_Impl);

   overriding
   procedure Find (Object  : in out Country_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Country_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Country_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Country_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Country_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Country_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Country_Ref'Class;
                        Impl   : out Country_Access);
   CITY_NAME : aliased constant String := "awa_city";
   COL_0_2_NAME : aliased constant String := "id";
   COL_1_2_NAME : aliased constant String := "name";
   COL_2_2_NAME : aliased constant String := "zip_code";
   COL_3_2_NAME : aliased constant String := "latitude";
   COL_4_2_NAME : aliased constant String := "longitude";
   COL_5_2_NAME : aliased constant String := "region_id";
   COL_6_2_NAME : aliased constant String := "country_id";

   CITY_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 7,
      Table   => CITY_NAME'Access,
      Members => (
         1 => COL_0_2_NAME'Access,
         2 => COL_1_2_NAME'Access,
         3 => COL_2_2_NAME'Access,
         4 => COL_3_2_NAME'Access,
         5 => COL_4_2_NAME'Access,
         6 => COL_5_2_NAME'Access,
         7 => COL_6_2_NAME'Access)
     );
   CITY_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := CITY_DEF'Access;


   Null_City : constant City_Ref
      := City_Ref'(ADO.Objects.Object_Ref with null record);

   type City_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => CITY_DEF'Access)
   with record
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Zip_Code : Integer;
       Latitude : Integer;
       Longitude : Integer;
       Region : Region_Ref;
       Country : Country_Ref;
   end record;

   type City_Access is access all City_Impl;

   overriding
   procedure Destroy (Object : access City_Impl);

   overriding
   procedure Find (Object  : in out City_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out City_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out City_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out City_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out City_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out City_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out City_Ref'Class;
                        Impl   : out City_Access);
   COUNTRY_NEIGHBOR_NAME : aliased constant String := "awa_country_neighbor";
   COL_0_3_NAME : aliased constant String := "id";
   COL_1_3_NAME : aliased constant String := "neighbor_of_id";
   COL_2_3_NAME : aliased constant String := "neighbor_id";

   COUNTRY_NEIGHBOR_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 3,
      Table   => COUNTRY_NEIGHBOR_NAME'Access,
      Members => (
         1 => COL_0_3_NAME'Access,
         2 => COL_1_3_NAME'Access,
         3 => COL_2_3_NAME'Access)
     );
   COUNTRY_NEIGHBOR_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := COUNTRY_NEIGHBOR_DEF'Access;


   Null_Country_Neighbor : constant Country_Neighbor_Ref
      := Country_Neighbor_Ref'(ADO.Objects.Object_Ref with null record);

   type Country_Neighbor_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => COUNTRY_NEIGHBOR_DEF'Access)
   with record
       Neighbor_Of : Country_Ref;
       Neighbor : Country_Ref;
   end record;

   type Country_Neighbor_Access is access all Country_Neighbor_Impl;

   overriding
   procedure Destroy (Object : access Country_Neighbor_Impl);

   overriding
   procedure Find (Object  : in out Country_Neighbor_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Country_Neighbor_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Country_Neighbor_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Country_Neighbor_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Country_Neighbor_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Country_Neighbor_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Country_Neighbor_Ref'Class;
                        Impl   : out Country_Neighbor_Access);
   REGION_NAME : aliased constant String := "awa_region";
   COL_0_4_NAME : aliased constant String := "id";
   COL_1_4_NAME : aliased constant String := "name";
   COL_2_4_NAME : aliased constant String := "geonameid";
   COL_3_4_NAME : aliased constant String := "country_id";

   REGION_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 4,
      Table   => REGION_NAME'Access,
      Members => (
         1 => COL_0_4_NAME'Access,
         2 => COL_1_4_NAME'Access,
         3 => COL_2_4_NAME'Access,
         4 => COL_3_4_NAME'Access)
     );
   REGION_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := REGION_DEF'Access;


   Null_Region : constant Region_Ref
      := Region_Ref'(ADO.Objects.Object_Ref with null record);

   type Region_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => REGION_DEF'Access)
   with record
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Geonameid : Integer;
       Country : Country_Ref;
   end record;

   type Region_Access is access all Region_Impl;

   overriding
   procedure Destroy (Object : access Region_Impl);

   overriding
   procedure Find (Object  : in out Region_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Region_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Region_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Region_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Region_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Region_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Region_Ref'Class;
                        Impl   : out Region_Access);
end AWA.Countries.Models;
