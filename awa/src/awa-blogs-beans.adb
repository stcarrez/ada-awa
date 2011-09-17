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

with AWA.Blogs.Services;

with ADO.Queries;
with ADO.Sessions;

with ASF.Applications.Messages.Factory;
with ASF.Events.Actions;
package body AWA.Blogs.Beans is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Example of action method.
   --  ------------------------------
   procedure Create_Post (Bean    : in out Post_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : AWA.Blogs.Services.Blog_Service_Access := Bean.Module.Get_Blog_Manager;
      Result  : ADO.Identifier;
   begin
      Manager.Create_Post (Blog_Id => Bean.Blog_Id,
                           Title   => Bean.Post.Get_Title,
                           URI     => Bean.Post.Get_URI,
                           Text    => Bean.Post.Get_Text,
                           Result  => Result);
      Outcome := To_Unbounded_String ("success");

   exception
      when Services.Not_Found =>
         Outcome := To_Unbounded_String ("failure");

         ASF.Applications.Messages.Factory.Add_Message ("users.signup_error_message");
   end Create_Post;

   package Create_Post_Binding is
     new ASF.Events.Actions.Action_Method.Bind (Bean   => Post_Bean,
                                                Method => Create_Post,
                                                Name   => "create");

   Post_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Create_Post_Binding.Proxy'Access);

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
      elsif Name = POST_TEXT_ATTR then
         return Util.Beans.Objects.To_Object (From.Text);
      elsif Name = POST_TITLE_ATTR then
         return Util.Beans.Objects.To_Object (From.Title);
      elsif Name = POST_URI_ATTR then
         return Util.Beans.Objects.To_Object (From.URI);
      else
         return Util.Beans.Objects.Null_Object;
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
         From.Blog_Id := ADO.Identifier (Util.Beans.Objects.To_Integer (Value));
      elsif Name = POST_TEXT_ATTR then
         From.Text := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = POST_TITLE_ATTR then
         From.Title := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = POST_URI_ATTR then
         From.URI := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;

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
   function Create_Post_Bean (Module : in AWA.Blogs.Module.Blog_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Post_Bean_Access := new Post_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Post_Bean;

   --  ------------------------------
   --  Create the Post_List_Bean bean instance.
   --  ------------------------------
   function Create_Post_List_Bean (Module : in AWA.Blogs.Module.Blog_Module_Access)
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

end AWA.Blogs.Beans;
