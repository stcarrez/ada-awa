-----------------------------------------------------------------------
--  awa-blogs-beans -- Beans for blog module
--  Copyright (C) 2011, 2012 Stephane Carrez
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
with AWA.Blogs.Services;
with AWA.Helpers.Requests;
with AWA.Helpers.Selectors;

with ADO.Queries;
with ADO.Sessions;
with ADO.Sessions.Entities;

with ASF.Applications.Messages.Factory;
with ASF.Events.Faces.Actions;
package body AWA.Blogs.Beans is

   use Ada.Strings.Unbounded;

   BLOG_ID_PARAMETER : constant String := "blog_id";
   POST_ID_PARAMETER : constant String := "post_id";

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

   package Create_Blog_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Blog_Bean,
                                                      Method => Create_Blog,
                                                      Name   => "create");

   Blog_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Create_Blog_Binding.Proxy'Access);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Blog_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Blog_Bean_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Create a new blog.
   --  ------------------------------
   procedure Create_Blog (Bean    : in out Blog_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : constant AWA.Blogs.Services.Blog_Service_Access := Bean.Module.Get_Blog_Manager;
      Result  : ADO.Identifier;
   begin
      Manager.Create_Blog (Workspace_Id => 0,
                           Title        => Bean.Get_Name,
                           Result       => Result);
      Outcome := To_Unbounded_String ("success");

   exception
      when Services.Not_Found =>
         Outcome := To_Unbounded_String ("failure");

         ASF.Applications.Messages.Factory.Add_Message ("users.signup_error_message");
   end Create_Blog;

   --  ------------------------------
   --  Create the Blog_Bean bean instance.
   --  ------------------------------
   function Create_Blog_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access is
      use type ADO.Identifier;

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
   --  Example of action method.
   --  ------------------------------
   procedure Create_Post (Bean    : in out Post_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type ADO.Identifier;

      Manager : constant AWA.Blogs.Services.Blog_Service_Access := Bean.Module.Get_Blog_Manager;
      Result  : ADO.Identifier;
      Blog_Id : constant ADO.Identifier := AWA.Helpers.Requests.Get_Parameter (BLOG_ID_PARAMETER);
      Post_Id : constant ADO.Identifier := AWA.Helpers.Requests.Get_Parameter (POST_ID_PARAMETER);
   begin
      if Post_Id < 0 then
         Manager.Create_Post (Blog_Id => Blog_Id,
                              Title   => Bean.Post.Get_Title,
                              URI     => Bean.Post.Get_Uri,
                              Text    => Bean.Post.Get_Text,
                              Status  => Bean.Post.Get_Status,
                              Result  => Result);
      else
         Manager.Update_Post (Post_Id => Post_Id,
                              Title   => Bean.Post.Get_Title,
                              Text    => Bean.Post.Get_Text,
                              Status  => Bean.Post.Get_Status);
      end if;
      Outcome := To_Unbounded_String ("success");

   exception
      when Services.Not_Found =>
         Outcome := To_Unbounded_String ("failure");

         ASF.Applications.Messages.Factory.Add_Message ("users.signup_error_message");
   end Create_Post;

   --  ------------------------------
   --  Delete a post.
   --  ------------------------------
   procedure Delete_Post (Bean    : in out Post_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : constant AWA.Blogs.Services.Blog_Service_Access := Bean.Module.Get_Blog_Manager;
   begin
      Manager.Delete_Post (Post_Id => Bean.Post.Get_Id);
      Outcome := To_Unbounded_String ("success");

   exception
      when Services.Not_Found =>
         Outcome := To_Unbounded_String ("failure");

         ASF.Applications.Messages.Factory.Add_Message ("users.signup_error_message");
   end Delete_Post;

   package Create_Post_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Post_Bean,
                                                      Method => Create_Post,
                                                      Name   => "create");

   package Save_Post_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Post_Bean,
                                                      Method => Create_Post,
                                                      Name   => "save");

   package Delete_Post_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Post_Bean,
                                                      Method => Delete_Post,
                                                      Name   => "delete");

   Post_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Create_Post_Binding.Proxy'Access,
         2 => Save_Post_Binding.Proxy'Access,
         3 => Delete_Post_Binding.Proxy'Access);

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Post_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = BLOG_ID_ATTR then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Blog_Id));
      elsif Name = POST_ID_ATTR then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Post.Get_Id));
      elsif Name = POST_USERNAME_ATTR then
         return Util.Beans.Objects.To_Object (String '(From.Post.Get_Author.Get_Name));
      else
         return From.Post.Get_Value (Name);
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
         From.Blog_Id := ADO.Identifier (Util.Beans.Objects.To_Integer (Value));
      elsif Name = POST_ID_ATTR then
         From.Load_Post (ADO.Identifier (Util.Beans.Objects.To_Integer (Value)));
      elsif Name = POST_TEXT_ATTR then
         From.Post.Set_Text (Util.Beans.Objects.To_Unbounded_String (Value));
      elsif Name = POST_TITLE_ATTR then
         From.Post.Set_Title (Util.Beans.Objects.To_Unbounded_String (Value));
      elsif Name = POST_URI_ATTR then
         From.Post.Set_Uri (Util.Beans.Objects.To_Unbounded_String (Value));
      elsif Name = POST_STATUS_ATTR then
         From.Post.Set_Status (AWA.Blogs.Models.Post_Status_Type_Objects.To_Value (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Load the post.
   --  ------------------------------
   procedure Load_Post (Post : in out Post_Bean;
                        Id   : in ADO.Identifier) is
      Session : ADO.Sessions.Session := Post.Module.Get_Session;
   begin
      Post.Post.Load (Session, Id);
      Post.Title := Post.Post.Get_Title;
      Post.Text  := Post.Post.Get_Text;
      Post.URI   := Post.Post.Get_Uri;

      --  SCz: 2012-05-19: workaround for ADO 0.3 limitation.  The lazy loading of
      --  objects does not work yet.  Force loading the user here while the above
      --  session is still open.
      declare
         A : constant String := String '(Post.Post.Get_Author.Get_Name);
         pragma Unreferenced (A);
      begin
         null;
      end;
   end Load_Post;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Post_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Post_Bean_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Create the Workspaces_Bean bean instance.
   --  ------------------------------
   function Create_Post_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access is
      use type ADO.Identifier;

      Object  : constant Post_Bean_Access := new Post_Bean;
--        Post_Id : constant ADO.Identifier := AWA.Helpers.Requests.Get_Parameter (POST_ID_PARAMETER);
   begin
--        if Post_Id > 0 then
--           declare
--              Session : ADO.Sessions.Session := Module.Get_Session;
--           begin
--              Object.Post.Load (Session, Post_Id);
--              Object.Title := Object.Post.Get_Title;
--              Object.Text  := Object.Post.Get_Text;
--              Object.URI   := Object.Post.Get_Uri;
--
--              --  SCz: 2012-05-19: workaround for ADO 0.3 limitation.  The lazy loading of
--              --  objects does not work yet.  Force loading the user here while the above
--              --  session is still open.
--              declare
--                 A : constant String := String '(Object.Post.Get_Author.Get_Name);
--                 pragma Unreferenced (A);
--              begin
--                 null;
--              end;
--           end;
--        end if;
      Object.Module := Module;
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
   --  Create the Admin_Post_List_Bean bean instance.
   --  ------------------------------
   function Create_Admin_Post_List_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                         return Util.Beans.Basic.Readonly_Bean_Access is
      use AWA.Blogs.Models;
      use AWA.Services;
      use type ADO.Identifier;

      Ctx   : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;

      Object  : constant Admin_Post_Info_List_Bean_Access := new Admin_Post_Info_List_Bean;
      Session : ADO.Sessions.Session := Module.Get_Session;
      Query   : ADO.Queries.Context;
      Blog_Id : constant ADO.Identifier := AWA.Helpers.Requests.Get_Parameter (BLOG_ID_PARAMETER);
   begin
      if Blog_Id > 0 then
         Query.Set_Query (AWA.Blogs.Models.Query_Blog_Admin_Post_List);
         Query.Bind_Param ("blog_id", Blog_Id);
         Query.Bind_Param ("user_id", User);
         ADO.Sessions.Entities.Bind_Param (Query, "table",
                                           AWA.Blogs.Models.BLOG_TABLE, Session);
         AWA.Blogs.Models.List (Object.all, Session, Query);
      end if;
      return Object.all'Access;
   end Create_Admin_Post_List_Bean;


   --  ------------------------------
   --  Create the Blog_List_Bean bean instance.
   --  ------------------------------
   function Create_Blog_List_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      use AWA.Blogs.Models;
      use AWA.Services;
      use type ADO.Identifier;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Object  : constant Blog_Info_List_Bean_Access := new Blog_Info_List_Bean;
      Session : ADO.Sessions.Session := Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Blogs.Models.Query_Blog_List);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Query, "table",
                                        AWA.Blogs.Models.BLOG_TABLE, Session);
      AWA.Blogs.Models.List (Object.all, Session, Query);
      return Object.all'Access;
   end Create_Blog_List_Bean;

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

end AWA.Blogs.Beans;
