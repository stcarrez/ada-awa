-----------------------------------------------------------------------
--  AWA.Blogs.Models -- AWA.Blogs.Models
-----------------------------------------------------------------------
--  File generated by ada-gen DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://ada-gen.googlecode.com/svn/trunk Revision 1095
-----------------------------------------------------------------------
--  Copyright (C) 2013 Stephane Carrez
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
pragma Warnings (Off, "unit * is not referenced");
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
with Util.Beans.Objects.Enums;
with Util.Beans.Basic.Lists;
with AWA.Users.Models;
with AWA.Workspaces.Models;
with Util.Beans.Methods;
pragma Warnings (On, "unit * is not referenced");
package AWA.Blogs.Models is
   type Post_Status_Type is (POST_DRAFT, POST_PUBLISHED, POST_SCHEDULED);
   for Post_Status_Type use (POST_DRAFT => 0, POST_PUBLISHED => 1, POST_SCHEDULED => 2);
   package Post_Status_Type_Objects is
      new Util.Beans.Objects.Enums (Post_Status_Type);

   type Blog_Ref is new ADO.Objects.Object_Ref with null record;

   type Post_Ref is new ADO.Objects.Object_Ref with null record;

   --  Create an object key for Blog.
   function Blog_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Blog from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Blog_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Blog : constant Blog_Ref;
   function "=" (Left, Right : Blog_Ref'Class) return Boolean;

   --  Set the blog identifier
   procedure Set_Id (Object : in out Blog_Ref;
                     Value  : in ADO.Identifier);

   --  Get the blog identifier
   function Get_Id (Object : in Blog_Ref)
                 return ADO.Identifier;

   --  Set the blog name
   procedure Set_Name (Object : in out Blog_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out Blog_Ref;
                       Value : in String);

   --  Get the blog name
   function Get_Name (Object : in Blog_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in Blog_Ref)
                 return String;
   --  Get the version
   function Get_Version (Object : in Blog_Ref)
                 return Integer;

   --  Set the blog uuid
   procedure Set_Uid (Object : in out Blog_Ref;
                      Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Uid (Object : in out Blog_Ref;
                      Value : in String);

   --  Get the blog uuid
   function Get_Uid (Object : in Blog_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Uid (Object : in Blog_Ref)
                 return String;

   --  Set the blog creation date
   procedure Set_Create_Date (Object : in out Blog_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the blog creation date
   function Get_Create_Date (Object : in Blog_Ref)
                 return Ada.Calendar.Time;

   --  Set the date when the blog was updated
   procedure Set_Update_Date (Object : in out Blog_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the date when the blog was updated
   function Get_Update_Date (Object : in Blog_Ref)
                 return Ada.Calendar.Time;

   --  Set the workspace that this blog belongs to
   procedure Set_Workspace (Object : in out Blog_Ref;
                            Value  : in AWA.Workspaces.Models.Workspace_Ref'Class);

   --  Get the workspace that this blog belongs to
   function Get_Workspace (Object : in Blog_Ref)
                 return AWA.Workspaces.Models.Workspace_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Blog_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Blog_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Blog_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Blog_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Blog_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Blog_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   BLOG_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Blog_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Blog_Ref;
                   Into   : in out Blog_Ref);

   package Blog_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Natural,
                                  Element_Type => Blog_Ref,
                                  "="          => "=");
   subtype Blog_Vector is Blog_Vectors.Vector;

   procedure List (Object  : in out Blog_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class);
   --  Create an object key for Post.
   function Post_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Post from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Post_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Post : constant Post_Ref;
   function "=" (Left, Right : Post_Ref'Class) return Boolean;

   --  Set the post identifier
   procedure Set_Id (Object : in out Post_Ref;
                     Value  : in ADO.Identifier);

   --  Get the post identifier
   function Get_Id (Object : in Post_Ref)
                 return ADO.Identifier;

   --  Set the post title
   procedure Set_Title (Object : in out Post_Ref;
                        Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Title (Object : in out Post_Ref;
                        Value : in String);

   --  Get the post title
   function Get_Title (Object : in Post_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Title (Object : in Post_Ref)
                 return String;

   --  Set the post text content
   procedure Set_Text (Object : in out Post_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Text (Object : in out Post_Ref;
                       Value : in String);

   --  Get the post text content
   function Get_Text (Object : in Post_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Text (Object : in Post_Ref)
                 return String;

   --  Set the post creation date
   procedure Set_Create_Date (Object : in out Post_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the post creation date
   function Get_Create_Date (Object : in Post_Ref)
                 return Ada.Calendar.Time;

   --  Set the post URI
   procedure Set_Uri (Object : in out Post_Ref;
                      Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Uri (Object : in out Post_Ref;
                      Value : in String);

   --  Get the post URI
   function Get_Uri (Object : in Post_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Uri (Object : in Post_Ref)
                 return String;
   --
   function Get_Version (Object : in Post_Ref)
                 return Integer;

   --  Set the post publication date
   procedure Set_Publish_Date (Object : in out Post_Ref;
                               Value  : in ADO.Nullable_Time);

   --  Get the post publication date
   function Get_Publish_Date (Object : in Post_Ref)
                 return ADO.Nullable_Time;

   --  Set the post status
   procedure Set_Status (Object : in out Post_Ref;
                         Value  : in Post_Status_Type);

   --  Get the post status
   function Get_Status (Object : in Post_Ref)
                 return Post_Status_Type;

   --
   procedure Set_Author (Object : in out Post_Ref;
                         Value  : in AWA.Users.Models.User_Ref'Class);

   --
   function Get_Author (Object : in Post_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --
   procedure Set_Blog (Object : in out Post_Ref;
                       Value  : in AWA.Blogs.Models.Blog_Ref'Class);

   --
   function Get_Blog (Object : in Post_Ref)
                 return AWA.Blogs.Models.Blog_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Post_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Post_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Post_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Post_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Post_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Post_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   POST_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Post_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Post_Ref;
                   Into   : in out Post_Ref);



   Query_Blog_Tag_Cloud : constant ADO.Queries.Query_Definition_Access;

   --  --------------------
   --  The Admin_Post_Info describes a post in the administration interface.
   --  --------------------
   type Admin_Post_Info is new Util.Beans.Basic.Readonly_Bean with record
      --  the post identifier.
      Id : ADO.Identifier;

      --  the post title.
      Title : Ada.Strings.Unbounded.Unbounded_String;

      --  the post uri.
      Uri : Ada.Strings.Unbounded.Unbounded_String;

      --  the post publish date.
      Date : Ada.Calendar.Time;

      --  the post status.
      Status : Post_Status_Type;

      --  the user name.
      Username : Ada.Strings.Unbounded.Unbounded_String;

   end record;

   --  Get the bean attribute identified by the given name.
   overriding
   function Get_Value (From : in Admin_Post_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   package Admin_Post_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Admin_Post_Info);
   package Admin_Post_Info_Vectors renames Admin_Post_Info_Beans.Vectors;
   subtype Admin_Post_Info_List_Bean is Admin_Post_Info_Beans.List_Bean;

   type Admin_Post_Info_List_Bean_Access is access all Admin_Post_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Admin_Post_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Admin_Post_Info_Vector is Admin_Post_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Admin_Post_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Blog_Admin_Post_List : constant ADO.Queries.Query_Definition_Access;

   --  --------------------
   --  The list of blogs.
   --  --------------------
   type Blog_Info is new Util.Beans.Basic.Readonly_Bean with record
      --  the blog identifier.
      Id : ADO.Identifier;

      --  the blog title.
      Title : Ada.Strings.Unbounded.Unbounded_String;

      --  the blog uuid.
      Uid : Ada.Strings.Unbounded.Unbounded_String;

      --  the blog creation date.
      Create_Date : Ada.Calendar.Time;

      --  the number of posts published.
      Post_Count : Integer;

   end record;

   --  Get the bean attribute identified by the given name.
   overriding
   function Get_Value (From : in Blog_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   package Blog_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Blog_Info);
   package Blog_Info_Vectors renames Blog_Info_Beans.Vectors;
   subtype Blog_Info_List_Bean is Blog_Info_Beans.List_Bean;

   type Blog_Info_List_Bean_Access is access all Blog_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Blog_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Blog_Info_Vector is Blog_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Blog_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Blog_List : constant ADO.Queries.Query_Definition_Access;

   --  --------------------
   --  The Post_Info describes a post to be displayed in the blog page
   --  --------------------
   type Post_Info is new Util.Beans.Basic.Readonly_Bean with record
      --  the post identifier.
      Id : ADO.Identifier;

      --  the post title.
      Title : Ada.Strings.Unbounded.Unbounded_String;

      --  the post uri.
      Uri : Ada.Strings.Unbounded.Unbounded_String;

      --  the post publish date.
      Date : Ada.Calendar.Time;

      --  the user name.
      Username : Ada.Strings.Unbounded.Unbounded_String;

      --  the post text.
      Text : Ada.Strings.Unbounded.Unbounded_String;

   end record;

   --  Get the bean attribute identified by the given name.
   overriding
   function Get_Value (From : in Post_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   package Post_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => Post_Info);
   package Post_Info_Vectors renames Post_Info_Beans.Vectors;
   subtype Post_Info_List_Bean is Post_Info_Beans.List_Bean;

   type Post_Info_List_Bean_Access is access all Post_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Post_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype Post_Info_Vector is Post_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out Post_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_Blog_Post_List : constant ADO.Queries.Query_Definition_Access;

   Query_Blog_Post_Tag_List : constant ADO.Queries.Query_Definition_Access;


   type Blog_Bean is abstract new AWA.Blogs.Models.Blog_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with null record;


   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Blog_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;


   --  Set the value identified by the name.
   overriding
   procedure Set_Value (Item  : in out Blog_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Create (Bean : in out Blog_Bean;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   type Post_Bean is abstract new AWA.Blogs.Models.Post_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with null record;


   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Post_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;


   --  Set the value identified by the name.
   overriding
   procedure Set_Value (Item  : in out Post_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Save (Bean : in out Post_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Delete (Bean : in out Post_Bean;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Load (Bean : in out Post_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;


private
   BLOG_NAME : aliased constant String := "awa_blog";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "name";
   COL_2_1_NAME : aliased constant String := "version";
   COL_3_1_NAME : aliased constant String := "uid";
   COL_4_1_NAME : aliased constant String := "create_date";
   COL_5_1_NAME : aliased constant String := "update_date";
   COL_6_1_NAME : aliased constant String := "workspace_id";

   BLOG_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count => 7,
      Table => BLOG_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access,
         6 => COL_5_1_NAME'Access,
         7 => COL_6_1_NAME'Access
)
     );
   BLOG_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := BLOG_DEF'Access;

   Null_Blog : constant Blog_Ref
      := Blog_Ref'(ADO.Objects.Object_Ref with others => <>);

   type Blog_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => BLOG_DEF'Access)
   with record
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Version : Integer;
       Uid : Ada.Strings.Unbounded.Unbounded_String;
       Create_Date : Ada.Calendar.Time;
       Update_Date : Ada.Calendar.Time;
       Workspace : AWA.Workspaces.Models.Workspace_Ref;
   end record;

   type Blog_Access is access all Blog_Impl;

   overriding
   procedure Destroy (Object : access Blog_Impl);

   overriding
   procedure Find (Object  : in out Blog_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Blog_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Blog_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Blog_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Blog_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Blog_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Blog_Ref'Class;
                        Impl   : out Blog_Access);
   POST_NAME : aliased constant String := "awa_post";
   COL_0_2_NAME : aliased constant String := "id";
   COL_1_2_NAME : aliased constant String := "title";
   COL_2_2_NAME : aliased constant String := "text";
   COL_3_2_NAME : aliased constant String := "create_date";
   COL_4_2_NAME : aliased constant String := "uri";
   COL_5_2_NAME : aliased constant String := "version";
   COL_6_2_NAME : aliased constant String := "publish_date";
   COL_7_2_NAME : aliased constant String := "status";
   COL_8_2_NAME : aliased constant String := "author_id";
   COL_9_2_NAME : aliased constant String := "blog_id";

   POST_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count => 10,
      Table => POST_NAME'Access,
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
         10 => COL_9_2_NAME'Access
)
     );
   POST_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := POST_DEF'Access;

   Null_Post : constant Post_Ref
      := Post_Ref'(ADO.Objects.Object_Ref with others => <>);

   type Post_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => POST_DEF'Access)
   with record
       Title : Ada.Strings.Unbounded.Unbounded_String;
       Text : Ada.Strings.Unbounded.Unbounded_String;
       Create_Date : Ada.Calendar.Time;
       Uri : Ada.Strings.Unbounded.Unbounded_String;
       Version : Integer;
       Publish_Date : ADO.Nullable_Time;
       Status : Post_Status_Type;
       Author : AWA.Users.Models.User_Ref;
       Blog : AWA.Blogs.Models.Blog_Ref;
   end record;

   type Post_Access is access all Post_Impl;

   overriding
   procedure Destroy (Object : access Post_Impl);

   overriding
   procedure Find (Object  : in out Post_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Post_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Post_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Post_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Post_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Post_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Post_Ref'Class;
                        Impl   : out Post_Access);

   package File_1 is
      new ADO.Queries.Loaders.File (Path => "blog-tags.xml",
                                    Sha1 => "9B2B599473F75F92CB5AB5045675E4CCEF926543");

   package Def_Blog_Tag_Cloud is
      new ADO.Queries.Loaders.Query (Name => "blog-tag-cloud",
                                     File => File_1.File'Access);
   Query_Blog_Tag_Cloud : constant ADO.Queries.Query_Definition_Access
   := Def_Blog_Tag_Cloud.Query'Access;

   package File_2 is
      new ADO.Queries.Loaders.File (Path => "blog-admin-post-list.xml",
                                    Sha1 => "F3BE5E28E359F698644846BAD351DA809BDBD95A");

   package Def_Adminpostinfo_Blog_Admin_Post_List is
      new ADO.Queries.Loaders.Query (Name => "blog-admin-post-list",
                                     File => File_2.File'Access);
   Query_Blog_Admin_Post_List : constant ADO.Queries.Query_Definition_Access
   := Def_Adminpostinfo_Blog_Admin_Post_List.Query'Access;

   package File_3 is
      new ADO.Queries.Loaders.File (Path => "blog-list.xml",
                                    Sha1 => "BB41EBE10B232F150560185E9A955BDA9FB7F77F");

   package Def_Bloginfo_Blog_List is
      new ADO.Queries.Loaders.Query (Name => "blog-list",
                                     File => File_3.File'Access);
   Query_Blog_List : constant ADO.Queries.Query_Definition_Access
   := Def_Bloginfo_Blog_List.Query'Access;

   package File_4 is
      new ADO.Queries.Loaders.File (Path => "blog-post-list.xml",
                                    Sha1 => "F61889EED51F4277BE7D979A3A2657A506FA0406");

   package Def_Postinfo_Blog_Post_List is
      new ADO.Queries.Loaders.Query (Name => "blog-post-list",
                                     File => File_4.File'Access);
   Query_Blog_Post_List : constant ADO.Queries.Query_Definition_Access
   := Def_Postinfo_Blog_Post_List.Query'Access;

   package Def_Postinfo_Blog_Post_Tag_List is
      new ADO.Queries.Loaders.Query (Name => "blog-post-tag-list",
                                     File => File_4.File'Access);
   Query_Blog_Post_Tag_List : constant ADO.Queries.Query_Definition_Access
   := Def_Postinfo_Blog_Post_Tag_List.Query'Access;
end AWA.Blogs.Models;
