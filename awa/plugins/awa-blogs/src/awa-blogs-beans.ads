-----------------------------------------------------------------------
--  awa-blogs-beans -- Beans for blog module
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
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

with Util.Beans.Basic;
with Util.Beans.Objects;

with ADO;

with AWA.Blogs.Modules;
with AWA.Blogs.Models;
with AWA.Tags.Beans;
package AWA.Blogs.Beans is

   --  Attributes exposed by <b>Post_Bean</b>
   BLOG_ID_ATTR      : constant String := "blogId";
   POST_ID_ATTR      : constant String := "id";
   POST_TITLE_ATTR   : constant String := "title";
   POST_TEXT_ATTR    : constant String := "text";
   POST_URI_ATTR     : constant String := "uri";
   POST_STATUS_ATTR  : constant String := "status";
   POST_USERNAME_ATTR : constant String := "username";
   POST_TAG_ATTR      : constant String := "tags";

   --  ------------------------------
   --  Blog Bean
   --  ------------------------------
   --  The <b>Blog_Bean</b> holds the information about the current blog.
   --  It allows to create the blog as well as update its primary title.
   type Blog_Bean is new AWA.Blogs.Models.Blog_Bean with record
      Module  : AWA.Blogs.Modules.Blog_Module_Access := null;
   end record;
   type Blog_Bean_Access is access all Blog_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Blog_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Blog_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create a new blog.
   overriding
   procedure Create (Bean    : in out Blog_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   type Post_Bean is new AWA.Blogs.Models.Post_Bean with record
      Module  : AWA.Blogs.Modules.Blog_Module_Access := null;
      Blog_Id : ADO.Identifier;

      --  List of tags associated with the post.
      Tags      : aliased AWA.Tags.Beans.Tag_List_Bean;
      Tags_Bean : Util.Beans.Basic.Readonly_Bean_Access;
   end record;
   type Post_Bean_Access is access all Post_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Post_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Post_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Load the post.
   procedure Load_Post (Post : in out Post_Bean;
                        Id   : in ADO.Identifier);

   --  Create or save the post.
   overriding
   procedure Save (Bean    : in out Post_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Delete a post.
   overriding
   procedure Delete (Bean    : in out Post_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Blog_Bean bean instance.
   function Create_Blog_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access;

   --  Create the Post_Bean bean instance.
   function Create_Post_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access;

   --  Create the Post_List_Bean bean instance.
   function Create_Post_List_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access;

   --  Create the Blog_Admin_Bean bean instance.
   function Create_Blog_Admin_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access;

   --  Get a select item list which contains a list of post status.
   function Create_Status_List (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access;

   type Init_Flag is (INIT_BLOG_LIST, INIT_POST_LIST);
   type Init_Map is array (Init_Flag) of Boolean;

   type Blog_Admin_Bean is new Util.Beans.Basic.Bean with record
      Module : AWA.Blogs.Modules.Blog_Module_Access := null;

      --  The blog identifier.
      Blog_Id          : ADO.Identifier := ADO.NO_IDENTIFIER;

      --  List of blogs.
      Blog_List        : aliased AWA.Blogs.Models.Blog_Info_List_Bean;
      Blog_List_Bean   : AWA.Blogs.Models.Blog_Info_List_Bean_Access;

      --  List of posts.
      Post_List        : aliased AWA.Blogs.Models.Admin_Post_Info_List_Bean;
      Post_List_Bean   : AWA.Blogs.Models.Admin_Post_Info_List_Bean_Access;

      --  Initialization flags.
      Init_Flags       : aliased Init_Map := (others => False);
      Flags            : access Init_Map;
   end record;
   type Blog_Admin_Bean_Access is access all Blog_Admin_Bean;

   --  Get the blog identifier.
   function Get_Blog_Id (List : in Blog_Admin_Bean) return ADO.Identifier;

   --  Load the posts associated with the current blog.
   procedure Load_Posts (List : in Blog_Admin_Bean);

   overriding
   function Get_Value (List : in Blog_Admin_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Blog_Admin_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Load the list of blogs.
   procedure Load_Blogs (List : in Blog_Admin_Bean);

end AWA.Blogs.Beans;
