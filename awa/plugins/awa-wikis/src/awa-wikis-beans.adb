-----------------------------------------------------------------------
--  awa-wikis-beans -- Beans for module wikis
--  Copyright (C) 2015, 2016 Stephane Carrez
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

with Util.Beans.Objects.Time;
with Util.Strings;

with ADO.Utils;
with ADO.Queries;
with ADO.Sessions;
with ADO.Datasets;
with ADO.Sessions.Entities;
with ADO.Parameters;
with ADO.Statements;

with ASF.Applications.Messages.Factory;

with AWA.Services;
with AWA.Services.Contexts;
with AWA.Tags.Modules;
with AWA.Helpers.Selectors;

with Wiki.Helpers;
package body AWA.Wikis.Beans is

   package ASC renames AWA.Services.Contexts;

   procedure Make_Image_Link (Renderer : in out Wiki_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              Info     : in AWA.Wikis.Models.Wiki_Image_Info;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : out Natural;
                              Height   : out Natural) is
      Sep : Natural;
   begin
      Sep := Wiki.Strings.Index (Link, "/");
      Width  := Info.Width;
      Height := Info.Height;
      URI := Renderer.Image_Prefix;
      Append (URI, Wiki.Strings.To_WString (Util.Strings.Image (Integer (Info.Id))));
      Append (URI, "/");
      if Sep = 0 then
         Append (URI, Link);
      else
         Append (URI, Link (Sep + 1 .. Link'Last));
      end if;
   end Make_Image_Link;

   procedure Find_Image_Link (Renderer : in out Wiki_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : out Natural;
                              Height   : out Natural) is
      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      List    : AWA.Wikis.Models.Wiki_Image_Info_Vector;
      Sep     : Natural;
      Query   : ADO.Queries.Context;
   begin
      Sep := Wiki.Strings.Index (Link, "/");
      Query.Bind_Param ("wiki_id", Renderer.Wiki_Space_Id);
      if Sep < 0 then
         Query.Bind_Param ("folder_name", String '("Images"));
         Sep := Link'First - 1;
      else
         Query.Bind_Param ("folder_name", Wiki.Strings.To_String (Link (Link'First .. Sep - 1)));
      end if;
      Query.Bind_Param ("file_name", Wiki.Strings.To_String (Link (Sep + 1 .. Link'Last)));
      Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Image);
      AWA.Wikis.Models.List (List, Session, Query);
      if not List.Is_Empty then
         declare
            Info : AWA.Wikis.Models.Wiki_Image_Info := List.First_Element;
         begin
            Renderer.Images.Include (Link, Info);
            Renderer.Make_Image_Link (Link, Info, URI, Width, Height);
         end;
      end if;
   end Find_Image_Link;

   --  ------------------------------
   --  Get the image link that must be rendered from the wiki image link.
   --  ------------------------------
   overriding
   procedure Make_Image_Link (Renderer : in out Wiki_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : out Natural;
                              Height   : out Natural) is
      Pos : Image_Info_Maps.Cursor;
   begin
      if Wiki.Helpers.Is_Url (Link) then
         URI    := To_Unbounded_Wide_Wide_String (Link);
         Width  := 0;
         Height := 0;
      else
         Pos := Renderer.Images.Find (Link);
         if Image_Info_Maps.Has_Element (Pos) then
            Renderer.Make_Image_Link (Link, Image_Info_Maps.Element (Pos), URI, Width, Height);
         else
            Renderer.Find_Image_Link (Link, URI, Width, Height);
         end if;
      end if;
   end Make_Image_Link;

   --  ------------------------------
   --  Get the page link that must be rendered from the wiki page link.
   --  ------------------------------
   overriding
   procedure Make_Page_Link (Renderer : in out Wiki_Links_Bean;
                             Link     : in Wiki.Strings.WString;
                             URI      : out Unbounded_Wide_Wide_String;
                             Exists   : out Boolean) is
   begin
      if Wiki.Helpers.Is_Url (Link) then
         URI := To_Unbounded_Wide_Wide_String (Link);
      else
         URI := Renderer.Image_Prefix;
         Append (URI, Link);
      end if;
      Exists := True;
   end Make_Page_Link;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Template_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (From, Name);
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Get the template content for the plugin evaluation.
   --  ------------------------------
   overriding
   procedure Get_Template (Plugin   : in out Wiki_Template_Bean;
                           Params   : in out Wiki.Attributes.Attribute_List;
                           Template : out Wiki.Strings.UString) is
      use Wiki.Strings;
      use type ADO.Identifier;
      package ASC renames AWA.Services.Contexts;

      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      Session : constant ADO.Sessions.Session := ASC.Get_Session (Ctx);
      First   : constant Wiki.Attributes.Cursor := Wiki.Attributes.First (Params);
      Name    : constant String := Wiki.Attributes.Get_Value (First);
      Query   : ADO.Queries.Context;
   begin
      Query.Bind_Param ("wiki_id", Plugin.Wiki_Space_Id);
      Query.Bind_Param ("name", "Template:" & Name);
      Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page_Content);
      declare
         Stmt   : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
         Result : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Stmt.Execute;
         if Stmt.Has_Elements then
            Result := Stmt.Get_Unbounded_String (0);
            Template := Wiki.Strings.To_UString
              (To_WString (Ada.Strings.Unbounded.To_String (Result)));
         end if;
      end;
   end Get_Template;

   --  Find a plugin knowing its name.
   overriding
   function Find (Factory : in Wiki_Template_Bean;
                  Name    : in String) return Wiki.Plugins.Wiki_Plugin_Access is
   begin
      return Factory'Unrestricted_Access;
   end Find;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_View_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      use type ADO.Identifier;
   begin
      if Name = "is_visible" then
         if From.Is_Public then
            return Util.Beans.Objects.To_Object (True);
         elsif From.Acl_Id /= ADO.NO_IDENTIFIER then
            return Util.Beans.Objects.To_Object (True);
         else
            return Util.Beans.Objects.To_Object (False);
         end if;
      elsif Name = "wiki_id" then
         return ADO.Utils.To_Object (From.Wiki_Space_Id);
      elsif Name = "tags" then
         return Util.Beans.Objects.To_Object (From.Tags_Bean, Util.Beans.Objects.STATIC);
      elsif Name = "links" then
         return Util.Beans.Objects.To_Object (From.Links_Bean, Util.Beans.Objects.STATIC);
      elsif Name = "plugins" then
         return Util.Beans.Objects.To_Object (From.Plugins_Bean, Util.Beans.Objects.STATIC);
      elsif Name = "counter" then
         return Util.Beans.Objects.To_Object (From.Counter_Bean, Util.Beans.Objects.STATIC);
      else
         return AWA.Wikis.Models.Wiki_View_Info (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_View_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "wiki_id" then
         From.Wiki_Space_Id := ADO.Utils.To_Identifier (Value);
         From.Plugins.Wiki_Space_Id := From.Wiki_Space_Id;
         From.Links.Wiki_Space_Id := From.Wiki_Space_Id;
         From.Links.Image_Prefix := From.Module.Get_Image_Prefix;
         Append (From.Links.Page_Prefix, Util.Beans.Objects.To_Wide_Wide_String (Value));
         Append (From.Links.Page_Prefix, "/");
         Append (From.Links.Image_Prefix, Util.Beans.Objects.To_Wide_Wide_String (Value));
         Append (From.Links.Image_Prefix, "/");
      else
         AWA.Wikis.Models.Wiki_View_Info (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Load the information about the wiki page to display it.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Wiki_View_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
      use type ADO.Identifier;
      package ASC renames AWA.Services.Contexts;

      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      Session : ADO.Sessions.Session := Bean.Module.Get_Session;
      Query   : ADO.Queries.Context;
   begin
      Query.Bind_Param ("wiki_id", Bean.Wiki_Space_Id);
      if Bean.Id /= ADO.NO_IDENTIFIER then
         Query.Bind_Param ("id", Bean.Id);
         Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page_Id);
      else
         Query.Bind_Param ("name", Bean.Name);
         Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page);
      end if;
      Query.Bind_Param ("user_id", Ctx.Get_User_Identifier);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "entity_type",
                                        Table   => AWA.Wikis.Models.WIKI_SPACE_TABLE,
                                        Session => Session);
      Bean.Load (Session, Query);

      if not Bean.Is_Public and Bean.Acl_Id <= 0 then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-visible");

      else
         --  Setup the wiki page read counter bean.
         ADO.Objects.Set_Value (Bean.Counter.Object, Bean.Id);
         Bean.Counter.Value := Bean.Read_Count;

         --  Load the wiki page tags.
         Bean.Tags.Load_Tags (Session, Bean.Id);
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");
      end if;
   end Load;

   --  ------------------------------
   --  Create the Wiki_View_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_View_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Wiki_View_Bean_Access := new Wiki_View_Bean;
   begin
      Object.Module := Module;
      Object.Tags_Bean := Object.Tags'Access;
      Object.Tags.Set_Entity_Type (AWA.Wikis.Models.WIKI_PAGE_TABLE);
      Object.Tags.Set_Permission ("wiki-page-update");
      Object.Counter_Bean := Object.Counter'Access;
      Object.Counter.Counter := AWA.Wikis.Modules.Read_Counter.Index;
      Object.Links_Bean := Object.Links'Access;
      Object.Plugins_Bean := Object.Plugins'Access;
      Object.Id := ADO.NO_IDENTIFIER;
      return Object.all'Access;
   end Create_Wiki_View_Bean;

   function Create_From_Format is
     new AWA.Helpers.Selectors.Create_From_Enum (AWA.Wikis.Models.Format_Type,
                                                 "wiki_format_");

   --  ------------------------------
   --  Get a select item list which contains a list of wiki formats.
   --  ------------------------------
   function Create_Format_List_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);
      use AWA.Helpers;
   begin
      return Selectors.Create_Selector_Bean (Bundle  => "wikis",
                                             Context => null,
                                             Create  => Create_From_Format'Access).all'Access;
   end Create_Format_List_Bean;

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
      if Name /= "id" then
         AWA.Wikis.Models.Wiki_Space_Bean (From).Set_Value (Name, Value);
      elsif Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Module.Load_Wiki_Space (From, ADO.Utils.To_Identifier (Value));
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
         Bean.Module.Save_Wiki_Space (Bean);
      else
         Bean.Module.Create_Wiki_Space (Bean);
      end if;
   end Save;

   --  ------------------------------
   --  Load the wiki space information.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Wiki_Space_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Load_Wiki_Space (Bean, Bean.Get_Id);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");
   end Load;

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
      Object.Module := Module;
      return Object.all'Access;
   end Create_Wiki_Space_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Page_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "wiki_id" then
         if From.Wiki_Space.Is_Null then
            return Util.Beans.Objects.Null_Object;
         else
            return ADO.Utils.To_Object (From.Wiki_Space.Get_Id);
         end if;
      elsif Name = "text" then
         if From.Has_Content then
            return Util.Beans.Objects.To_Object (From.New_Content);
         elsif From.Content.Is_Null then
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
      elsif Name = "format" then
         if not From.Content.Is_Null then
            return From.Content.Get_Value ("format");
         elsif not From.Wiki_Space.Is_Null then
            return From.Wiki_Space.Get_Value ("format");
         else
            return Util.Beans.Objects.Null_Object;
         end if;
      elsif Name = "comment" then
         if From.Content.Is_Null then
            return Util.Beans.Objects.Null_Object;
         else
            return Util.Beans.Objects.To_Object (String '(From.Content.Get_Save_Comment));
         end if;
      elsif Name = "tags" then
         return Util.Beans.Objects.To_Object (From.Tags_Bean, Util.Beans.Objects.STATIC);
      elsif Name = "is_public" then
         if not From.Is_Null then
            return AWA.Wikis.Models.Wiki_Page_Bean (From).Get_Value (Name);
         elsif not From.Wiki_Space.Is_Null then
            return From.Wiki_Space.Get_Value (Name);
         else
            return Util.Beans.Objects.Null_Object;
         end if;
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
         if not Util.Beans.Objects.Is_Empty (Value) then
            declare
               Id  : constant ADO.Identifier := ADO.Utils.To_Identifier (Value);
            begin
               From.Module.Load_Page (From, From.Content, From.Tags, Id);
            end;
         end if;
      elsif Name = "wiki_id" then
         From.Wiki_Space.Set_Id (ADO.Utils.To_Identifier (Value));
      elsif Name = "text" then
         From.Has_Content := True;
         From.New_Content := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "format" then
         From.Format := AWA.Wikis.Models.Format_Type_Objects.To_Value (Value);
      elsif Name = "comment" then
         From.New_Comment := Util.Beans.Objects.To_Unbounded_String (Value);
         From.Content.Set_Save_Comment (Util.Beans.Objects.To_String (Value));
      else
         AWA.Wikis.Models.Wiki_Page_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Returns True if the wiki page has a new text content and requires
   --  a new version to be created.
   --  ------------------------------
   function Has_New_Content (Bean : in Wiki_Page_Bean) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type AWA.Wikis.Models.Format_Type;
   begin
      if not Bean.Is_Inserted then
         return True;
      elsif not Bean.Has_Content then
         return False;
      elsif Bean.Content.Get_Format /= Bean.Format then
         return True;
      else
         declare
            Current : constant Ada.Strings.Unbounded.Unbounded_String := Bean.Content.Get_Content;
         begin
            return Current /= Bean.New_Content;
         end;
      end if;
   end Has_New_Content;

   --  ------------------------------
   --  Create or save the wiki page.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Wiki_Page_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use ASF.Applications;

      Result : ADO.Identifier;
   begin
      if not Bean.Is_Inserted then
         Bean.Content.Set_Content (Bean.New_Content);
         Bean.Content.Set_Save_Comment (Bean.New_Comment);
         Bean.Content.Set_Format (Bean.Format);
         Bean.Module.Create_Wiki_Page (Bean.Wiki_Space, Bean, Bean.Content);

      elsif not Bean.Has_New_Content then
         Bean.Module.Save (Bean);

      else
         Bean.Content := AWA.Wikis.Models.Null_Wiki_Content;
         Bean.Content.Set_Content (Bean.New_Content);
         Bean.Content.Set_Save_Comment (Bean.New_Comment);
         Bean.Content.Set_Format (Bean.Format);
         Bean.Module.Create_Wiki_Content (Bean, Bean.Content);
      end if;
      Result := Bean.Get_Id;
      Bean.Tags.Update_Tags (Result);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");

   exception
      when AWA.Wikis.Modules.Name_Used =>
         Messages.Factory.Add_Field_Message ("name", "wikis.wiki_page_name_used");
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("failure");

   end Save;

   --  ------------------------------
   --  Load the wiki page.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Wiki_Page_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Load_Page (Bean, Bean.Content, Bean.Tags,
                             Bean.Wiki_Space.Get_Id, Bean.Get_Name);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");

   exception
      when ADO.Objects.NOT_FOUND =>
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
   end Load;

   --  ------------------------------
   --  Setup the wiki page for the creation.
   --  ------------------------------
   overriding
   procedure Setup (Bean    : in out Wiki_Page_Bean;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Load_Wiki_Space (Bean.Wiki_Space, Bean.Wiki_Space.Get_Id);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");

   exception
      when ADO.Objects.NOT_FOUND =>
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
   end Setup;

   --  ------------------------------
   --  Delete the wiki page.
   --  ------------------------------
   overriding
   procedure Delete (Bean    : in out Wiki_Page_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Delete (Bean);
   end Delete;

   --  ------------------------------
   --  Create the Wiki_Page_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_Page_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Wiki_Page_Bean_Access := new Wiki_Page_Bean;
   begin
      Object.Module    := Module;
      Object.Tags_Bean := Object.Tags'Access;
      Object.Tags.Set_Entity_Type (AWA.Wikis.Models.WIKI_PAGE_TABLE);
      Object.Tags.Set_Permission ("wiki-page-update");
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

      elsif Name = "sort" then
         return Util.Beans.Objects.To_Object (From.Sort);

      elsif Name = "count" then
         return Util.Beans.Objects.To_Object (From.Count);

      elsif Name = "page_count" then
         return Util.Beans.Objects.To_Object ((From.Count + From.Page_Size - 1) / From.Page_Size);

      elsif Name = "wiki_id" then
         return ADO.Utils.To_Object (From.Wiki_Id);

      elsif Name = "update_date" then
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
      elsif Name = "sort" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Sort := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;

   overriding
   procedure Load (From    : in out Wiki_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      From.Load_List;
   end Load;

   --  ------------------------------
   --  Load the list of pages.  If a tag was set, filter the list of pages with the tag.
   --  ------------------------------
   procedure Load_List (Into : in out Wiki_List_Bean) is
      use AWA.Wikis.Models;
      use AWA.Services;
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type ADO.Identifier;

      Ctx         : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User        : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session     : ADO.Sessions.Session := Into.Module.Get_Session;
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
         Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page_Tag_List);
         Query.Bind_Param (Name => "tag", Value => Tag_Id);
         Count_Query.Set_Count_Query (AWA.Wikis.Models.Query_Wiki_Page_Tag_List);
         Count_Query.Bind_Param (Name => "tag", Value => Tag_Id);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "page_table",
                                           Table   => AWA.Wikis.Models.WIKI_PAGE_TABLE,
                                           Session => Session);
         ADO.Sessions.Entities.Bind_Param (Params  => Count_Query,
                                           Name    => "page_table",
                                           Table   => AWA.Wikis.Models.WIKI_PAGE_TABLE,
                                           Session => Session);
      else
         Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page_List);
         Count_Query.Set_Count_Query (AWA.Wikis.Models.Query_Wiki_Page_List);
      end if;
      if Into.Sort = "name" then
         Query.Bind_Param (Name  => "order1",
                           Value => ADO.Parameters.Token '("page.name"));
      elsif Into.Sort = "recent" then
         Query.Bind_Param (Name  => "order1",
                           Value => ADO.Parameters.Token '("content.create_date"));
      elsif Into.Sort = "popular" then
         Query.Bind_Param (Name  => "order1",
                           Value => ADO.Parameters.Token '("page.read_count"));
      else
         return;
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
      Object.Module     := Module;
      Object.Pages_Bean := Object.Pages'Access;
      Object.Page_Size  := 20;
      Object.Page       := 1;
      Object.Count      := 0;
      Object.Wiki_Id    := ADO.NO_IDENTIFIER;
      return Object.all'Access;
   end Create_Wiki_List_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Version_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "versions" then
         return Util.Beans.Objects.To_Object (Value   => From.Versions_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "page_count" then
         return Util.Beans.Objects.To_Object ((From.Count + From.Page_Size - 1) / From.Page_Size);
      else
         return AWA.Wikis.Models.Wiki_Version_List_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_Version_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "page" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Page := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "wiki_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Wiki_Id := ADO.Utils.To_Identifier (Value);
      elsif Name = "page_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Page_Id := ADO.Utils.To_Identifier (Value);
      end if;
   end Set_Value;

   overriding
   procedure Load (Into    : in out Wiki_Version_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use AWA.Wikis.Models;
      use AWA.Services;
      use type ADO.Identifier;
      use type Ada.Strings.Unbounded.Unbounded_String;

      Ctx         : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User        : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session     : ADO.Sessions.Session := Into.Module.Get_Session;
      Query       : ADO.Queries.Context;
      Count_Query : ADO.Queries.Context;
      First       : constant Natural  := (Into.Page - 1) * Into.Page_Size;
      Page        : constant Wiki_View_Bean_Access := Get_Wiki_View_Bean ("wikiView");
   begin
      if Into.Wiki_Id = ADO.NO_IDENTIFIER or Into.Page_Id = ADO.NO_IDENTIFIER then
         return;
      end if;

      --  Load the wiki page first.
      Page.Wiki_Space_Id := Into.Wiki_Id;
      Page.Id := Into.Page_Id;
      Page.Load (Outcome);
      if Outcome /= "loaded" then
         return;
      end if;

      --  Get the list of versions associated with the wiki page.
      Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Version_List);
      Count_Query.Set_Count_Query (AWA.Wikis.Models.Query_Wiki_Version_List);
      Query.Bind_Param (Name => "first", Value => First);
      Query.Bind_Param (Name => "count", Value => Into.Page_Size);
      Query.Bind_Param (Name => "wiki_id", Value => Into.Wiki_Id);
      Query.Bind_Param (Name => "page_id", Value => Into.Page_Id);
      Query.Bind_Param (Name => "user_id", Value => User);
      Count_Query.Bind_Param (Name => "wiki_id", Value => Into.Wiki_Id);
      Count_Query.Bind_Param (Name => "page_id", Value => Into.Page_Id);
      Count_Query.Bind_Param (Name => "user_id", Value => User);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "table",
                                        Table   => AWA.Wikis.Models.WIKI_SPACE_TABLE,
                                        Session => Session);
      ADO.Sessions.Entities.Bind_Param (Params  => Count_Query,
                                        Name    => "table",
                                        Table   => AWA.Wikis.Models.WIKI_SPACE_TABLE,
                                        Session => Session);
      AWA.Wikis.Models.List (Into.Versions, Session, Query);
      Into.Count := ADO.Datasets.Get_Count (Session, Count_Query);
   end Load;

   --  ------------------------------
   --  Create the Post_List_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_Version_List_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                           return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Wiki_Version_List_Bean_Access := new Wiki_Version_List_Bean;
   begin
      Object.Module     := Module;
      Object.Versions_Bean := Object.Versions'Access;
      Object.Page_Size  := 20;
      Object.Page       := 1;
      Object.Count      := 0;
      Object.Wiki_Id    := ADO.NO_IDENTIFIER;
      Object.Page_Id    := ADO.NO_IDENTIFIER;
      return Object.all'Access;
   end Create_Wiki_Version_List_Bean;

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
