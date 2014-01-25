-----------------------------------------------------------------------
--  awa-blogs-beans -- Beans for blog module
--  Copyright (C) 2011, 2012, 2013, 2014 Stephane Carrez
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
with Ada.Calendar;
with Ada.Characters.Handling;

with Util.Dates.ISO8601;

with AWA.Services.Contexts;
with AWA.Helpers.Requests;
with AWA.Helpers.Selectors;
with AWA.Tags.Modules;
with AWA.Comments.Beans;

with ADO.Utils;
with ADO.Queries;
with ADO.SQL;
with ADO.Sessions;
with ADO.Sessions.Entities;

package body AWA.Blogs.Beans is

   use type ADO.Identifier;
   use Ada.Strings.Unbounded;

   BLOG_ID_PARAMETER : constant String := "blog_id";

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Blog_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if not From.Is_Null then
         return AWA.Blogs.Models.Blog_Ref (From).Get_Value (Name);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Blog_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "name" then
         From.Set_Name (Util.Beans.Objects.To_String (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create a new blog.
   --  ------------------------------
   procedure Create (Bean    : in out Blog_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      Result  : ADO.Identifier;
      pragma Unreferenced (Result);
   begin
      Bean.Module.Create_Blog (Workspace_Id => 0,
                               Title        => Bean.Get_Name,
                               Result       => Result);
   end Create;

   --  ------------------------------
   --  Create the Blog_Bean bean instance.
   --  ------------------------------
   function Create_Blog_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access is

      Blog_Id : constant ADO.Identifier := AWA.Helpers.Requests.Get_Parameter (BLOG_ID_PARAMETER);
      Object  : constant Blog_Bean_Access := new Blog_Bean;
      Session : ADO.Sessions.Session := Module.Get_Session;
   begin
      if Blog_Id /= ADO.NO_IDENTIFIER then
         Object.Load (Session, Blog_Id);
      end if;
      Object.Module := Module;
      return Object.all'Access;
   end Create_Blog_Bean;

   --  ------------------------------
   --  Build the URI from the post title and the post date.
   --  ------------------------------
   function Get_Predefined_Uri (Title : in String;
                                Date  : in Ada.Calendar.Time) return String is
      D      : constant String := Util.Dates.ISO8601.Image (Date);
      Result : String (1 .. Title'Length + 11);
   begin
      Result (1 .. 4) := D (1 .. 4);
      Result (5) := '/';
      Result (6 .. 7) := D (6 .. 7);
      Result (8) := '/';
      Result (9 .. 10) := D (9 .. 10);
      Result (11) := '/';
      for I in Title'Range loop
         if Ada.Characters.Handling.Is_Alphanumeric (Title (I))
           or Title (I) = '-' or Title (I) = '_' or Title (I) = '$' or Title (I) = ','
           or Title (I) = '.' or Title (I) = '+' then
            Result (I + 11) := Title (I);
         else
            Result (I + 11) := '-';
         end if;
      end loop;
      return Result;
   end Get_Predefined_Uri;

   --  ------------------------------
   --  Create or save the post.
   --  ------------------------------
   procedure Save (Bean    : in out Post_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      Result  : ADO.Identifier;
   begin
      if not Bean.Is_Inserted then
         Bean.Module.Create_Post (Blog_Id => Bean.Blog_Id,
                                  Title   => Bean.Get_Title,
                                  URI     => Get_Predefined_Uri (Bean.Get_Title,
                                    Ada.Calendar.Clock),
                                  Text    => Bean.Get_Text,
                                  Status  => Bean.Get_Status,
                                  Result  => Result);
      else
         Bean.Module.Update_Post (Post_Id => Bean.Get_Id,
                                  Title   => Bean.Get_Title,
                                  URI     => Bean.Get_Uri,
                                  Text    => Bean.Get_Text,
                                  Status  => Bean.Get_Status);
         Result := Bean.Get_Id;
      end if;
      Bean.Tags.Update_Tags (Result);
   end Save;

   --  ------------------------------
   --  Delete a post.
   --  ------------------------------
   procedure Delete (Bean    : in out Post_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Delete_Post (Post_Id => Bean.Get_Id);
   end Delete;

   --  ------------------------------
   --  Load the post from the URI.
   --  ------------------------------
   overriding
   procedure Load (bean    : in out Post_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type AWA.Comments.Beans.Comment_Bean_Access;
      use type AWA.Comments.Beans.Comment_List_Bean_Access;

      Session      : ADO.Sessions.Session := Bean.Module.Get_Session;
      Query        : ADO.SQL.Query;
      Found        : Boolean;
      Comment      : AWA.Comments.Beans.Comment_Bean_Access;
      Comment_List : AWA.Comments.Beans.Comment_List_Bean_Access;
   begin
      Query.Bind_Param (1, String '(Bean.Get_Uri));
      Query.Set_Filter ("o.uri = ?");
      Bean.Find (Session, Query, Found);
      if not Found then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
         return;
      end if;
      Bean.Tags.Load_Tags (Session, Bean.Get_Id);

      Comment := AWA.Comments.Beans.Get_Comment_Bean ("postNewComment");
      if Comment /= null then
         Comment.Set_Entity_Id (Bean.Get_Id);
      end if;
      Comment_List := AWA.Comments.Beans.Get_Comment_List_Bean ("postComments");
      if Comment_List /= null then
         Comment_List.Load_Comments (Bean.Get_Id);
      end if;

      --  SCz: 2012-05-19: workaround for ADO 0.3 limitation.  The lazy loading of
      --  objects does not work yet.  Force loading the user here while the above
      --  session is still open.
      declare
         A : constant String := String '(Bean.Get_Author.Get_Name);
         pragma Unreferenced (A);
      begin
         null;
      end;
   end Load;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Post_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = BLOG_ID_ATTR then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Blog_Id));
      elsif Name = POST_TAG_ATTR then
         return Util.Beans.Objects.To_Object (From.Tags_Bean, Util.Beans.Objects.STATIC);
      elsif From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      elsif Name = POST_ID_ATTR then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Get_Id));
      elsif Name = POST_USERNAME_ATTR then
         return Util.Beans.Objects.To_Object (String '(From.Get_Author.Get_Name));
      else
         return AWA.Blogs.Models.Post_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Post_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = BLOG_ID_ATTR then
         From.Blog_Id := ADO.Utils.To_Identifier (Value);
      elsif Name = POST_ID_ATTR and not Util.Beans.Objects.Is_Empty (Value) then
         From.Load_Post (ADO.Utils.To_Identifier (Value));
      elsif Name = POST_TEXT_ATTR then
         From.Set_Text (Util.Beans.Objects.To_Unbounded_String (Value));
      elsif Name = POST_TITLE_ATTR then
         From.Set_Title (Util.Beans.Objects.To_Unbounded_String (Value));
      elsif Name = POST_URI_ATTR then
         From.Set_Uri (Util.Beans.Objects.To_Unbounded_String (Value));
      elsif Name = POST_STATUS_ATTR then
         From.Set_Status (AWA.Blogs.Models.Post_Status_Type_Objects.To_Value (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Load the post.
   --  ------------------------------
   procedure Load_Post (Post : in out Post_Bean;
                        Id   : in ADO.Identifier) is
      Session : ADO.Sessions.Session := Post.Module.Get_Session;
   begin
      Post.Load (Session, Id);
      Post.Tags.Load_Tags (Session, Id);

      --  SCz: 2012-05-19: workaround for ADO 0.3 limitation.  The lazy loading of
      --  objects does not work yet.  Force loading the user here while the above
      --  session is still open.
      declare
         A : constant String := String '(Post.Get_Author.Get_Name);
         pragma Unreferenced (A);
      begin
         null;
      end;
   end Load_Post;

   --  ------------------------------
   --  Create the Workspaces_Bean bean instance.
   --  ------------------------------
   function Create_Post_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Post_Bean_Access := new Post_Bean;
   begin
      Object.Module := Module;
      Object.Tags_Bean := Object.Tags'Access;
      Object.Tags.Set_Entity_Type (AWA.Blogs.Models.POST_TABLE);
      Object.Tags.Set_Permission ("blog-update-post");
      return Object.all'Access;
   end Create_Post_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Post_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      Pos : Natural;
   begin
      if Name = "tags" then
         Pos := From.Posts.Get_Row_Index;
         if Pos = 0 then
            return Util.Beans.Objects.Null_Object;
         end if;
         declare
            Item : constant Models.Post_Info := From.Posts.List.Element (Pos - 1);
         begin
            return From.Tags.Get_Tags (Item.Id);
         end;
      elsif Name = "posts" then
         return Util.Beans.Objects.To_Object (Value   => From.Posts_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "tag" then
         return Util.Beans.Objects.To_Object (From.Tag);
      else
         return From.Posts.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Post_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "tag" then
         From.Tag := Util.Beans.Objects.To_Unbounded_String (Value);
         From.Load_List;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Load the list of posts.  If a tag was set, filter the list of posts with the tag.
   --  ------------------------------
   procedure Load_List (Into : in out Post_List_Bean) is
      use AWA.Blogs.Models;
      use AWA.Services;

      Session : ADO.Sessions.Session := Into.Service.Get_Session;
      Query   : ADO.Queries.Context;
      Tag_Id  : ADO.Identifier;
   begin
      AWA.Tags.Modules.Find_Tag_Id (Session, Ada.Strings.Unbounded.To_String (Into.Tag), Tag_Id);
      if Tag_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (AWA.Blogs.Models.Query_Blog_Post_Tag_List);
         Query.Bind_Param (Name => "tag", Value => Tag_Id);
      else
         Query.Set_Query (AWA.Blogs.Models.Query_Blog_Post_List);
      end if;
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "entity_type",
                                        Table   => AWA.Blogs.Models.POST_TABLE,
                                        Session => Session);
      AWA.Blogs.Models.List (Into.Posts, Session, Query);
      declare
         List : ADO.Utils.Identifier_Vector;
         Iter : Post_Info_Vectors.Cursor := Into.Posts.List.First;
      begin
         while Post_Info_Vectors.Has_Element (Iter) loop
            List.Append (Post_Info_Vectors.Element (Iter).Id);
            Post_Info_Vectors.Next (Iter);
         end loop;
         Into.Tags.Load_Tags (Session, AWA.Blogs.Models.POST_TABLE.Table.all,
                              List);
      end;
   end Load_List;

   --  ------------------------------
   --  Create the Post_List_Bean bean instance.
   --  ------------------------------
   function Create_Post_List_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Post_List_Bean_Access := new Post_List_Bean;
   begin
      Object.Service    := Module;
      Object.Posts_Bean := Object.Posts'Access;
      Object.Load_List;
      return Object.all'Access;
   end Create_Post_List_Bean;

   --  ------------------------------
   --  Create the Blog_List_Bean bean instance.
   --  ------------------------------
   function Create_Blog_Admin_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Blog_Admin_Bean_Access := new Blog_Admin_Bean;
   begin
      Object.Module            := Module;
      Object.Flags             := Object.Init_Flags'Access;
      Object.Post_List_Bean    := Object.Post_List'Access;
      Object.Blog_List_Bean    := Object.Blog_List'Access;
      Object.Comment_List_Bean := Object.Comment_List'Access;
      return Object.all'Access;
   end Create_Blog_Admin_Bean;

   function Create_From_Status is
     new AWA.Helpers.Selectors.Create_From_Enum (AWA.Blogs.Models.Post_Status_Type,
                                                 "blog_status_");

   --  ------------------------------
   --  Get a select item list which contains a list of post status.
   --  ------------------------------
   function Create_Status_List (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);
      use AWA.Helpers;
   begin
      return Selectors.Create_Selector_Bean (Bundle  => "blogs",
                                             Context => null,
                                             Create  => Create_From_Status'Access).all'Access;
   end Create_Status_List;

   --  ------------------------------
   --  Load the list of blogs.
   --  ------------------------------
   procedure Load_Blogs (List : in Blog_Admin_Bean) is
      use AWA.Blogs.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := List.Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Blogs.Models.Query_Blog_List);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "table",
                                        Table   => AWA.Blogs.Models.BLOG_TABLE,
                                        Session => Session);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "entity_type",
                                        Table   => AWA.Blogs.Models.POST_TABLE,
                                        Session => Session);
      AWA.Blogs.Models.List (List.Blog_List_Bean.all, Session, Query);
      List.Flags (INIT_BLOG_LIST) := True;
   end Load_Blogs;

   --  ------------------------------
   --  Get the blog identifier.
   --  ------------------------------
   function Get_Blog_Id (List : in Blog_Admin_Bean) return ADO.Identifier is
   begin
      if List.Blog_Id = ADO.NO_IDENTIFIER then
         if not List.Flags (INIT_BLOG_LIST) then
            Load_Blogs (List);
         end if;
         if not List.Blog_List.List.Is_Empty then
            return List.Blog_List.List.Element (0).Id;
         end if;
      end if;
      return List.Blog_Id;
   end Get_Blog_Id;

   --  ------------------------------
   --  Load the posts associated with the current blog.
   --  ------------------------------
   procedure Load_Posts (List : in Blog_Admin_Bean) is
      use AWA.Blogs.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := List.Module.Get_Session;
      Query   : ADO.Queries.Context;
      Blog_Id : constant ADO.Identifier := List.Get_Blog_Id;
   begin
      if Blog_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (AWA.Blogs.Models.Query_Blog_Admin_Post_List);
         Query.Bind_Param ("blog_id", Blog_Id);
         Query.Bind_Param ("user_id", User);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "table",
                                           Table   => AWA.Blogs.Models.BLOG_TABLE,
                                           Session => Session);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "entity_type",
                                           Table   => AWA.Blogs.Models.POST_TABLE,
                                           Session => Session);

         AWA.Blogs.Models.List (List.Post_List_Bean.all, Session, Query);
         List.Flags (INIT_POST_LIST) := True;
      end if;
   end Load_Posts;

   --  ------------------------------
   --  Load the comments associated with the current blog.
   --  ------------------------------
   procedure Load_Comments (List : in Blog_Admin_Bean) is
      use AWA.Blogs.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := List.Module.Get_Session;
      Query   : ADO.Queries.Context;
      Blog_Id : constant ADO.Identifier := List.Get_Blog_Id;
   begin
      if Blog_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (AWA.Blogs.Models.Query_Comment_List);
         Query.Bind_Param ("blog_id", Blog_Id);
         Query.Bind_Param ("user_id", User);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "table",
                                           Table   => AWA.Blogs.Models.BLOG_TABLE,
                                           Session => Session);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "entity_type",
                                           Table   => AWA.Blogs.Models.POST_TABLE,
                                           Session => Session);

         AWA.Blogs.Models.List (List.Comment_List_Bean.all, Session, Query);
         List.Flags (INIT_COMMENT_LIST) := True;
      end if;
   end Load_Comments;

   overriding
   function Get_Value (List : in Blog_Admin_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "blogs" then
         if not List.Init_Flags (INIT_BLOG_LIST) then
            Load_Blogs (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Blog_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "posts" then
         if not List.Init_Flags (INIT_POST_LIST) then
            Load_Posts (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Post_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "comments" then
         if not List.Init_Flags (INIT_COMMENT_LIST) then
            Load_Comments (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Comment_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "id" then
         declare
            Id : constant ADO.Identifier := List.Get_Blog_Id;
         begin
            if Id = ADO.NO_IDENTIFIER then
               return Util.Beans.Objects.Null_Object;
            else
               return Util.Beans.Objects.To_Object (Long_Long_Integer (Id));
            end if;
         end;
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Blog_Admin_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Blog_Id := ADO.Utils.To_Identifier (Value);
      end if;
   end Set_Value;

end AWA.Blogs.Beans;
