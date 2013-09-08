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

with AWA.Services.Contexts;
with AWA.Helpers.Requests;
with AWA.Helpers.Selectors;

with ADO.Utils;
with ADO.Queries;
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
                                  URI     => Bean.Get_Uri,
                                  Text    => Bean.Get_Text,
                                  Status  => Bean.Get_Status,
                                  Result  => Result);
      else
         Bean.Module.Update_Post (Post_Id => Bean.Get_Id,
                                  Title   => Bean.Get_Title,
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
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Post_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = BLOG_ID_ATTR then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Blog_Id));
      elsif From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      elsif Name = POST_ID_ATTR then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Get_Id));
      elsif Name = POST_USERNAME_ATTR then
         return Util.Beans.Objects.To_Object (String '(From.Get_Author.Get_Name));
      elsif Name = POST_TAG_ATTR then
         return Util.Beans.Objects.To_Object (From.Tags_Bean, Util.Beans.Objects.STATIC);
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
   --  Create the Post_List_Bean bean instance.
   --  ------------------------------
   function Create_Post_List_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      use AWA.Blogs.Models;

      Object  : constant Post_Info_List_Bean_Access := new Post_Info_List_Bean;
      Session : ADO.Sessions.Session := Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Blogs.Models.Query_Blog_Post_List);
      AWA.Blogs.Models.List (Object.all, Session, Query);
      return Object.all'Access;
   end Create_Post_List_Bean;

   --  ------------------------------
   --  Create the Blog_List_Bean bean instance.
   --  ------------------------------
   function Create_Blog_Admin_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Blog_Admin_Bean_Access := new Blog_Admin_Bean;
   begin
      Object.Module         := Module;
      Object.Flags          := Object.Init_Flags'Access;
      Object.Post_List_Bean := Object.Post_List'Access;
      Object.Blog_List_Bean := Object.Blog_List'Access;
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
      ADO.Sessions.Entities.Bind_Param (Query, "table",
                                        AWA.Blogs.Models.BLOG_TABLE, Session);
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
         ADO.Sessions.Entities.Bind_Param (Query, "table",
                                           AWA.Blogs.Models.BLOG_TABLE, Session);

         AWA.Blogs.Models.List (List.Post_List_Bean.all, Session, Query);
         List.Flags (INIT_POST_LIST) := True;
      end if;
   end Load_Posts;

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
