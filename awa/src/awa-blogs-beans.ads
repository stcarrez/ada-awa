-----------------------------------------------------------------------
--  awa-blogs-beans -- Beans for blog module
--  Copyright (C) 2011 Stephane Carrez
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
with Util.Beans.Methods;

with ADO;

with AWA.Blogs.Module;
with AWA.Blogs.Models;
package Awa.Blogs.Beans is

   --  Attributes exposed by <b>Post_Bean</b>
   BLOG_ID_ATTR    : constant String := "blogId";
   POST_ID_ATTR    : constant String := "id";
   POST_TITLE_ATTR : constant String := "title";
   POST_TEXT_ATTR  : constant String := "text";
   POST_URI_ATTR   : constant String := "uri";

   type Post_Bean is new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Module  : AWA.Blogs.Module.Blog_Module_Access := null;
      Post    : AWA.Blogs.Models.Post_Ref;
      Blog_Id : ADO.Identifier;
      Title   : Ada.Strings.Unbounded.Unbounded_String;
      Text    : Ada.Strings.Unbounded.Unbounded_String;
      URI     : Ada.Strings.Unbounded.Unbounded_String;
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

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Post_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Create a new post.
   procedure Create_Post (Bean    : in out Post_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Workspaces_Bean bean instance.
   function Create_Post_Bean (Module : in AWA.Blogs.Module.Blog_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access;

   --  Create the Post_List_Bean bean instance.
   function Create_Post_List_Bean (Module : in AWA.Blogs.Module.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access;

   --  Create the Admin_Post_List_Bean bean instance.
   function Create_Admin_Post_List_Bean (Module : in AWA.Blogs.Module.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Blogs.Beans;
