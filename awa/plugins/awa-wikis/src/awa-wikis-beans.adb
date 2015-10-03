-----------------------------------------------------------------------
--  awa-wikis-beans -- Beans for module wikis
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
with ADO.Utils;
with ADO.Queries;
with ADO.Sessions;
with ADO.Objects;
with ADO.Datasets;
with ADO.Sessions.Entities;

with AWA.Services;
with AWA.Services.Contexts;
with AWA.Tags.Modules;

package body AWA.Wikis.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Space_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return AWA.Wikis.Models.Wiki_Space_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_Space_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "name" then
         From.Set_Name (Util.Beans.Objects.To_String (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create or save the wiki space.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Wiki_Space_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      if Bean.Is_Inserted then
         Bean.Service.Save_Wiki_Space (Bean);
      else
         Bean.Service.Create_Wiki_Space (Bean);
      end if;
   end Save;

   --  Delete the wiki space.
   procedure Delete (Bean    : in out Wiki_Space_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Delete;

   --  ------------------------------
   --  Create the Wiki_Space_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_Space_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Wiki_Space_Bean_Access := new Wiki_Space_Bean;
   begin
      Object.Service   := Module;
      return Object.all'Access;
   end Create_Wiki_Space_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Page_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "wikiId" then
         if From.Wiki_Space.Is_Null then
            return Util.Beans.Objects.Null_Object;
         else
            return ADO.Utils.To_Object (From.Wiki_Space.Get_Id);
         end if;
      elsif Name = "text" then
         if From.Content.Is_Null then
            return Util.Beans.Objects.Null_Object;
         else
            return Util.Beans.Objects.To_Object (String '(From.Content.Get_Content));
         end if;
      elsif Name = "date" then
         if From.Content.Is_Null then
            return Util.Beans.Objects.Null_Object;
         else
            return From.Content.Get_Value ("create_date");
         end if;
      elsif Name = "comment" then
         if From.Content.Is_Null then
            return Util.Beans.Objects.Null_Object;
         else
            return Util.Beans.Objects.To_Object (String '(From.Content.Get_Save_Comment));
         end if;
      elsif Name = "tags" then
         return Util.Beans.Objects.To_Object (From.Tags_Bean, Util.Beans.Objects.STATIC);
      elsif From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return AWA.Wikis.Models.Wiki_Page_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_Page_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" then
         declare
            Id  : constant ADO.Identifier := ADO.Utils.To_Identifier (Value);
         begin
            From.Service.Load_Page (From, From.Content, From.Tags, Id);
         end;
      elsif Name = "wikiId" then
         From.Wiki_Space.Set_Id (ADO.Utils.To_Identifier (Value));
      elsif Name = "name" then
         From.Set_Name (Util.Beans.Objects.To_String (Value));
      elsif Name = "title" then
         From.Set_Title (Util.Beans.Objects.To_String (Value));
      elsif Name = "is_public" then
         From.Set_Is_Public (Util.Beans.Objects.To_Boolean (Value));
      elsif Name = "text" then
         From.Has_Content := True;
         if From.Content.Is_Inserted then
            From.Content := AWA.Wikis.Models.Null_Wiki_Content;
         end if;
         From.Content.Set_Content (Util.Beans.Objects.To_String (Value));
      elsif Name = "comment" then
         From.Content.Set_Save_Comment (Util.Beans.Objects.To_String (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create or save the wiki page.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Wiki_Page_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Bean.Is_Inserted then
         Bean.Service.Save (Bean);
      else
         Bean.Service.Create_Wiki_Page (Bean.Wiki_Space, Bean);
      end if;
      if Bean.Has_Content then
         Bean.Service.Create_Wiki_Content (Bean, Bean.Content);
      end if;
   end Save;

   --  ------------------------------
   --  Load the wiki page.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Wiki_Page_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Service.Load_Page (Bean, Bean.Content, Bean.Tags,
                              Bean.Wiki_Space.Get_Id, Bean.Get_Name);

   exception
      when ADO.Objects.NOT_FOUND =>
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
   end Load;

   --  Delete the wiki page.
   overriding
   procedure Delete (Bean    : in out Wiki_Page_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Delete;

   --  ------------------------------
   --  Create the Wiki_Page_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_Page_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Wiki_Page_Bean_Access := new Wiki_Page_Bean;
   begin
      Object.Service   := Module;
      Object.Tags_Bean := Object.Tags'Access;
      Object.Tags.Set_Entity_Type (AWA.Wikis.Models.WIKI_PAGE_TABLE);
      return Object.all'Access;
   end Create_Wiki_Page_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      Pos : Natural;
   begin
      if Name = "tags" then
         Pos := From.Pages.Get_Row_Index;
         if Pos = 0 then
            return Util.Beans.Objects.Null_Object;
         end if;
         declare
            Item : constant Models.Wiki_Page_Info := From.Pages.List.Element (Pos - 1);
         begin
            return From.Tags.Get_Tags (Item.Id);
         end;
      elsif Name = "pages" then
         return Util.Beans.Objects.To_Object (Value   => From.Pages_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "tag" then
         return Util.Beans.Objects.To_Object (From.Tag);

      elsif Name = "page" then
         return Util.Beans.Objects.To_Object (From.Page);

      elsif Name = "count" then
         return Util.Beans.Objects.To_Object (From.Count);

      elsif Name = "page_count" then
         return Util.Beans.Objects.To_Object ((From.Count + From.Page_Size - 1) / From.Page_Size);

      elsif Name = "wiki_id" then
         return ADO.Utils.To_Object (From.Wiki_Id);

      elsif Name = "updateDate" then
         if From.Pages.Get_Count = 0 then
            return Util.Beans.Objects.Null_Object;
         else
            declare
               Item : constant Models.Wiki_Page_Info := From.Pages.List.Element (0);
            begin
               return Util.Beans.Objects.Time.To_Object (Item.Create_Date);
            end;
         end if;
      else
         return From.Pages.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "tag" then
         From.Tag := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "page" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Page := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "wiki_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Wiki_Id := ADO.Utils.To_Identifier (Value);
      end if;
   end Set_Value;

   overriding
   procedure Load (From    : in out Wiki_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      From.Load_List;
   end Load;

   --  ------------------------------
   --  Load the list of pages.  If a tag was set, filter the list of pages with the tag.
   --  ------------------------------
   procedure Load_List (Into : in out Wiki_List_Bean) is
      use AWA.Wikis.Models;
      use AWA.Services;
      use type ADO.Identifier;

      Ctx         : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User        : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session     : ADO.Sessions.Session := Into.Service.Get_Session;
      Query       : ADO.Queries.Context;
      Count_Query : ADO.Queries.Context;
      Tag_Id      : ADO.Identifier;
      First       : constant Natural  := (Into.Page - 1) * Into.Page_Size;
   begin
      if Into.Wiki_Id = ADO.NO_IDENTIFIER then
         return;
      end if;
      AWA.Tags.Modules.Find_Tag_Id (Session, Ada.Strings.Unbounded.To_String (Into.Tag), Tag_Id);
      if Tag_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page_List);
         Query.Bind_Param (Name => "tag", Value => Tag_Id);
         Count_Query.Set_Count_Query (AWA.Wikis.Models.Query_Wiki_Page_List);
         Count_Query.Bind_Param (Name => "tag", Value => Tag_Id);
      else
         Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page_List);
         Count_Query.Set_Count_Query (AWA.Wikis.Models.Query_Wiki_Page_List);
      end if;
      Query.Bind_Param (Name => "first", Value => First);
      Query.Bind_Param (Name => "count", Value => Into.Page_Size);
      Query.Bind_Param (Name => "wiki_id", Value => Into.Wiki_Id);
      Query.Bind_Param (Name => "user_id", Value => User);
      Count_Query.Bind_Param (Name => "wiki_id", Value => Into.Wiki_Id);
      Count_Query.Bind_Param (Name => "user_id", Value => User);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "table",
                                        Table   => AWA.Wikis.Models.WIKI_SPACE_TABLE,
                                        Session => Session);
      ADO.Sessions.Entities.Bind_Param (Params  => Count_Query,
                                        Name    => "table",
                                        Table   => AWA.Wikis.Models.WIKI_SPACE_TABLE,
                                        Session => Session);
      AWA.Wikis.Models.List (Into.Pages, Session, Query);
      Into.Count := ADO.Datasets.Get_Count (Session, Count_Query);
      declare
         List : ADO.Utils.Identifier_Vector;
         Iter : Wiki_Page_Info_Vectors.Cursor := Into.Pages.List.First;
      begin
         while Wiki_Page_Info_Vectors.Has_Element (Iter) loop
            List.Append (Wiki_Page_Info_Vectors.Element (Iter).Id);
            Wiki_Page_Info_Vectors.Next (Iter);
         end loop;
         Into.Tags.Load_Tags (Session, AWA.Wikis.Models.WIKI_PAGE_TABLE.Table.all,
                              List);
      end;
   end Load_List;

   --  ------------------------------
   --  Create the Wiki_List_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_List_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Wiki_List_Bean_Access := new Wiki_List_Bean;
   begin
      Object.Service    := Module;
      Object.Pages_Bean := Object.Pages'Access;
      Object.Page_Size  := 20;
      Object.Page       := 1;
      Object.Count      := 0;
      Object.Wiki_Id    := ADO.NO_IDENTIFIER;
      return Object.all'Access;
   end Create_Wiki_List_Bean;

   --  ------------------------------
   --  Load the list of wikis.
   --  ------------------------------
   procedure Load_Wikis (List : in Wiki_Admin_Bean) is
      use AWA.Wikis.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := List.Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Wikis.Models.Query_Wiki_List);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "table",
                                        Table   => AWA.Wikis.Models.WIKI_SPACE_TABLE,
                                        Session => Session);
      AWA.Wikis.Models.List (List.Wiki_List_Bean.all, Session, Query);
      List.Flags (INIT_WIKI_LIST) := True;
   end Load_Wikis;

   --  ------------------------------
   --  Get the wiki space identifier.
   --  ------------------------------
   function Get_Wiki_Id (List : in Wiki_Admin_Bean) return ADO.Identifier is
      use type ADO.Identifier;
   begin
      if List.Wiki_Id = ADO.NO_IDENTIFIER then
         if not List.Flags (INIT_WIKI_LIST) then
            Load_Wikis (List);
         end if;
         if not List.Wiki_List.List.Is_Empty then
            return List.Wiki_List.List.Element (0).Id;
         end if;
      end if;
      return List.Wiki_Id;
   end Get_Wiki_Id;

   overriding
   function Get_Value (List : in Wiki_Admin_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "wikis" then
         if not List.Init_Flags (INIT_WIKI_LIST) then
            Load_Wikis (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Wiki_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "id" then
         declare
            use type ADO.Identifier;

            Id : constant ADO.Identifier := List.Get_Wiki_Id;
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

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_Admin_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Wiki_Id := ADO.Utils.To_Identifier (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create the Wiki_Admin_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_Admin_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Wiki_Admin_Bean_Access := new Wiki_Admin_Bean;
   begin
      Object.Module            := Module;
      Object.Flags             := Object.Init_Flags'Access;
      Object.Wiki_List_Bean    := Object.Wiki_List'Access;
      return Object.all'Access;
   end Create_Wiki_Admin_Bean;

end AWA.Wikis.Beans;
