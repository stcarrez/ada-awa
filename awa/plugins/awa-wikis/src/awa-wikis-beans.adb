-----------------------------------------------------------------------
--  awa-wikis-beans -- Beans for module wikis
--  Copyright (C) 2015, 2016, 2017, 2018, 2019, 2020 Stephane Carrez
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
with AWA.Tags.Models;
with AWA.Helpers.Selectors;
with AWA.Images.Modules;

with Wiki.Documents;
with Wiki.Parsers;
with Wiki.Filters.Html;
with Wiki.Filters.Autolink;
with Wiki.Filters.Collectors;
with Wiki.Helpers;
package body AWA.Wikis.Beans is

   package ASC renames AWA.Services.Contexts;

   procedure Make_Image_Link (Renderer : in out Wiki_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              Info     : in AWA.Wikis.Models.Wiki_Image_Info;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural) is
      Sep : Natural;
   begin
      Sep := Wiki.Strings.Index (Link, "/");
      URI := Renderer.Image_Prefix;
      Append (URI, Wiki.Strings.To_WString (Util.Strings.Image (Integer (Info.Id))));
      Append (URI, "/");
      if Width = 0 and Height = 0 then
         Append (URI, "default/");
      elsif Width = Natural'Last or Height = Natural'Last then
         Append (URI, "original/");
      else
         if Width /= 0 then
            Append (URI, Wiki.Strings.To_WString (Util.Strings.Image (Width)));
         end if;
         Append (URI, "x");
         if Height /= 0 then
            Append (URI, Wiki.Strings.To_WString (Util.Strings.Image (Height)));
         end if;
         Append (URI, "/");
      end if;
      if Sep = 0 then
         Append (URI, Link);
      else
         Append (URI, Link (Sep + 1 .. Link'Last));
      end if;
      if not Info.Width.Is_Null and not Info.Height.Is_Null then
         AWA.Images.Modules.Scale (Width     => Info.Width.Value,
                                   Height    => Info.Height.Value,
                                   To_Width  => Width,
                                   To_Height => Height);
      end if;
   end Make_Image_Link;

   procedure Find_Image_Link (Renderer : in out Wiki_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural) is
      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      List    : AWA.Wikis.Models.Wiki_Image_Info_Vector;
      Sep     : Natural;
      Query   : ADO.Queries.Context;
      Info    : AWA.Wikis.Models.Wiki_Image_Info;
   begin
      Sep := Wiki.Strings.Index (Link, "/");
      Query.Bind_Param ("wiki_id", Renderer.Wiki_Space_Id);
      if Sep = 0 then
         Query.Bind_Param ("folder_name", String '("Images"));
         Sep := Link'First - 1;
      else
         Query.Bind_Param ("folder_name", Wiki.Strings.To_String (Link (Link'First .. Sep - 1)));
      end if;
      Query.Bind_Param ("file_name", Wiki.Strings.To_String (Link (Sep + 1 .. Link'Last)));
      Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Image);
      AWA.Wikis.Models.List (List, Session, Query);
      if not List.Is_Empty then
         Info := List.First_Element;
      else
         Info.Id     := ADO.NO_IDENTIFIER;
         Info.Width.Is_Null := True;
         Info.Height.Is_Null := True;
      end if;
      Renderer.Images.Include (Link, Info);
      Renderer.Make_Image_Link (Link, Info, URI, Width, Height);
   end Find_Image_Link;

   --  ------------------------------
   --  Get the image link that must be rendered from the wiki image link.
   --  ------------------------------
   overriding
   procedure Make_Image_Link (Renderer : in out Wiki_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural) is
      Pos : Image_Info_Maps.Cursor;
   begin
      if Wiki.Helpers.Is_Url (Link) then
         URI    := To_Unbounded_Wide_Wide_String (Link);
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
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Links_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (Integer (From.Images.Length));
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : Wiki_Links_Bean) return Natural is
   begin
      return Natural (From.Images.Length);
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out Wiki_Links_Bean;
                            Index : in Natural) is
   begin
      if Index = 1 then
         From.Pos := From.Images.First;
      else
         Image_Info_Maps.Next (From.Pos);
      end if;
      From.Info := Image_Info_Maps.Element (From.Pos);
      From.Info_Bean := From.Info'Unchecked_Access;
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : in Wiki_Links_Bean) return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.To_Object (Value   => From.Info_Bean,
                                           Storage => Util.Beans.Objects.STATIC);
   end Get_Row;

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
         URI := Renderer.Page_Prefix;
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
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (Natural (From.Templates.Length));
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : in Wiki_Template_Bean) return Natural is
   begin
      return Natural (From.Templates.Length);
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out Wiki_Template_Bean;
                            Index : in Natural) is
   begin
      if Index = 1 then
         From.Pos := From.Templates.First;
      else
         Template_Maps.Next (From.Pos);
      end if;
      From.Info.Is_Public := False;
      From.Info.Id := ADO.NO_IDENTIFIER;
      From.Info.Name := Ada.Strings.Unbounded.To_Unbounded_String (Template_Maps.Key (From.Pos));
      From.Info_Bean := From.Info'Unchecked_Access;
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : in Wiki_Template_Bean) return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.To_Object (Value   => From.Info_Bean,
                                           Storage => Util.Beans.Objects.STATIC);
   end Get_Row;

   --  ------------------------------
   --  Get the template content for the plugin evaluation.
   --  ------------------------------
   overriding
   procedure Get_Template (Plugin   : in out Wiki_Template_Bean;
                           Params   : in out Wiki.Attributes.Attribute_List;
                           Template : out Wiki.Strings.UString) is
      use Wiki.Strings;
      package ASC renames AWA.Services.Contexts;

      First   : constant Wiki.Attributes.Cursor := Wiki.Attributes.First (Params);
      Name    : constant String := "Template:" & Wiki.Attributes.Get_Value (First);
      Pos     : constant Template_Maps.Cursor := Plugin.Templates.Find (Name);
      Query   : ADO.Queries.Context;
   begin
      if Template_Maps.Has_Element (Pos) then
         Template := Template_Maps.Element (Pos);
      else
         Query.Bind_Param ("wiki_id", Plugin.Wiki_Space_Id);
         Query.Bind_Param ("name", Name);
         Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page_Content);
         declare
            Ctx     : constant ASC.Service_Context_Access := ASC.Current;
            Session : constant ADO.Sessions.Session := ASC.Get_Session (Ctx);
            Stmt    : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
            Result  : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Stmt.Execute;
            if Stmt.Has_Elements then
               Result := Stmt.Get_Unbounded_String (0);
               Template := Wiki.Strings.To_UString
                 (To_WString (Ada.Strings.Unbounded.To_String (Result)));
            end if;
            Plugin.Templates.Include (Name, Template);
         end;
      end if;
   end Get_Template;

   --  ------------------------------
   --  Find a plugin knowing its name.
   --  ------------------------------
   overriding
   function Find (Factory : in Wiki_Template_Bean;
                  Name    : in String) return Wiki.Plugins.Wiki_Plugin_Access is
   begin
      if Name = "if" or Name = "else" or Name = "elsif" or Name = "end" then
         return Factory.Condition'Unrestricted_Access;
      elsif Name = "set" then
         return Factory.Variable'Unrestricted_Access;
      else
         return Factory'Unrestricted_Access;
      end if;
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
         if From.Is_Public.Is_Null or else From.Acl_Id /= ADO.NO_IDENTIFIER then
            return Util.Beans.Objects.To_Object (False);
         else
            return Util.Beans.Objects.To_Object (From.Is_Public.Value);
         end if;
      elsif Name = "wiki_id" then
         return ADO.Utils.To_Object (From.Wiki_Space.Get_Id);
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
   --  Set the wiki identifier.
   --  ------------------------------
   procedure Set_Wiki_Id (Into : in out Wiki_View_Bean;
                          Id   : in ADO.Identifier) is
      S : constant Wide_Wide_String := ADO.Identifier'Wide_Wide_Image (Id);
   begin
      Into.Wiki_Space.Set_Id (Id);
      Into.Plugins.Wiki_Space_Id := Id;
      Into.Links.Wiki_Space_Id := Id;
      Into.Links.Page_Prefix  := Into.Module.Get_Page_Prefix;
      Into.Links.Image_Prefix := Into.Module.Get_Image_Prefix;
      Append (Into.Links.Page_Prefix, S (S'First + 1 .. S'Last));
      Append (Into.Links.Page_Prefix, "/");
      Append (Into.Links.Image_Prefix, S (S'First + 1 .. S'Last));
      Append (Into.Links.Image_Prefix, "/");
   end Set_Wiki_Id;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_View_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "wiki_id" then
         From.Set_Wiki_Id (ADO.Utils.To_Identifier (Value));
      else
         AWA.Wikis.Models.Wiki_View_Info (From).Set_Value (Name, Value);
      end if;

   exception
      when Constraint_Error =>
         From.Set_Wiki_Id (ADO.NO_IDENTIFIER);

   end Set_Value;

   --  ------------------------------
   --  Get the wiki syntax for the page.
   --  ------------------------------
   function Get_Syntax (From : in Wiki_View_Bean) return Wiki.Wiki_Syntax is
   begin
      case (if From.Format.Is_Null then From.Side_Format else From.Format.Value) is
         when Models.FORMAT_CREOLE =>
            return Wiki.SYNTAX_CREOLE;
         when Models.FORMAT_MARKDOWN =>
            return Wiki.SYNTAX_MARKDOWN;
         when Models.FORMAT_HTML =>
            return Wiki.SYNTAX_HTML;
         when Models.FORMAT_DOTCLEAR =>
            return Wiki.SYNTAX_DOTCLEAR;
         when Models.FORMAT_MEDIAWIKI =>
            return Wiki.SYNTAX_MEDIA_WIKI;
         when Models.FORMAT_PHPBB =>
            return Wiki.SYNTAX_PHPBB;
      end case;
   end Get_Syntax;

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
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
      Name    : constant Ada.Strings.Unbounded.Unbounded_String := Bean.Name.Value;
   begin
      if Bean.Wiki_Space.Is_Null then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
         return;
      end if;
      Query.Bind_Param ("wiki_id", Bean.Wiki_Space.Get_Id);
      if Bean.Id /= ADO.NO_IDENTIFIER then
         Query.Bind_Param ("id", Bean.Id);
         Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page_Id);
      else
         Query.Bind_Param ("name", Name);
         Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Page);
      end if;
      Query.Bind_Param ("user_id", Ctx.Get_User_Identifier);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "entity_type",
                                        Table   => AWA.Wikis.Models.WIKI_SPACE_TABLE,
                                        Session => Session);
      Bean.Load (Session, Query);

      if Bean.Id <= 0 or Bean.Is_Public.Is_Null then
         Bean.Name.Is_Null := False;
         Bean.Name.Value := Name;
         Bean.Title.Is_Null := False;
         Bean.Title.Value := Name;
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-created");

      elsif not Bean.Is_Public.Value and Bean.Acl_Id <= 0 then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-visible");

      else
         --  Setup the wiki page read counter bean.
         ADO.Objects.Set_Value (Bean.Counter.Object, Bean.Id);
         Bean.Counter.Value := Bean.Read_Count.Value;

         --  Load the wiki page tags.
         Bean.Tags.Load_Tags (Session, Bean.Id);
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");

         if Ctx.Get_User_Identifier /= ADO.NO_IDENTIFIER then
            Bean.Plugins.Condition.Append ("user", "");
            Bean.Plugins.Condition.Append ("authentified", "");
         else
            Bean.Plugins.Condition.Append ("anonymous", "");
         end if;
         if Bean.Is_Public.Value then
            Bean.Plugins.Condition.Append ("public", "");
         else
            Bean.Plugins.Condition.Append ("private", "");
         end if;
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
      Object.Wiki_Space := Get_Wiki_Space_Bean ("adminWikiSpace");
      --  Object.Wiki_Space_Id := ADO.NO_IDENTIFIER;
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

   exception
      when others =>
         null;
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
         Bean.Module.Create_Wiki_Page (Bean.Wiki_Space.all, Bean, Bean.Content);

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
      Bean.Module.Load_Wiki_Space (Bean.Wiki_Space.all, Bean.Wiki_Space.Get_Id);
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
      Object.Wiki_Space := Get_Wiki_Space_Bean ("adminWikiSpace");
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
            Item : constant Models.Wiki_Page_Info := From.Pages.List.Element (Pos);
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
               Item : constant Models.Wiki_Page_Info := From.Pages.List.Element (1);
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
         From.Page := 1;
         From.Page := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "wiki_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Wiki_Id := ADO.NO_IDENTIFIER;
         From.Wiki_Id := ADO.Utils.To_Identifier (Value);
      elsif Name = "sort" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Sort := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;

   exception
      when Constraint_Error =>
         null;
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
      Session     : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query       : ADO.Queries.Context;
      Count_Query : ADO.Queries.Context;
      Tag_Id      : ADO.Identifier;
      First       : constant Natural  := (Into.Page - 1) * Into.Page_Size;
      Tag         : constant String := Ada.Strings.Unbounded.To_String (Into.Tag);
   begin
      if Into.Wiki_Id = ADO.NO_IDENTIFIER then
         return;
      end if;
      Into.Wiki_Space.Set_Id (Into.Wiki_Id);
      if Tag'Length > 0 then
         AWA.Tags.Modules.Find_Tag_Id (Session, Tag, Tag_Id);
         if Tag_Id = ADO.NO_IDENTIFIER then
            return;
         end if;
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
                           Value => ADO.Parameters.Token '("content.create_date DESC"));
      elsif Into.Sort = "popular" then
         Query.Bind_Param (Name  => "order1",
                           Value => ADO.Parameters.Token '("page.read_count DESC"));
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
      Object.Wiki_Space := Get_Wiki_Space_Bean ("adminWikiSpace");
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
         From.Wiki_Id := ADO.NO_IDENTIFIER;
         From.Wiki_Id := ADO.Utils.To_Identifier (Value);
      elsif Name = "page_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Page_Id := ADO.NO_IDENTIFIER;
         From.Page_Id := ADO.Utils.To_Identifier (Value);
      end if;

   exception
      when Constraint_Error =>
         null;
   end Set_Value;

   overriding
   procedure Load (Into    : in out Wiki_Version_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use AWA.Wikis.Models;
      use AWA.Services;
      use type Ada.Strings.Unbounded.Unbounded_String;

      Ctx         : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User        : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session     : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query       : ADO.Queries.Context;
      Count_Query : ADO.Queries.Context;
      First       : constant Natural  := (Into.Page - 1) * Into.Page_Size;
      Page        : constant Wiki_View_Bean_Access := Get_Wiki_View_Bean ("wikiView");
   begin
      --  Load the wiki page first.
      Page.Id := Into.Page_Id;
      Page.Set_Wiki_Id (Into.Wiki_Id);
      Page.Load (Outcome);
      if Outcome /= "loaded" then
         return;
      end if;
      Page.Wiki_Space.Set_Id (Into.Wiki_Id);

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
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Page_Info_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "words" then
         return Util.Beans.Objects.To_Object (Value   => From.Words_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "pages" then
         return Util.Beans.Objects.To_Object (Value   => From.Links_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "links" then
         return Util.Beans.Objects.To_Object (Value   => From.Ext_Links_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "images" then
         return Util.Beans.Objects.To_Object (Value   => From.Page.Links_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "templates" then
         return Util.Beans.Objects.To_Object (Value   => From.Page.Plugins_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "imageThumbnail" then
         declare
            URI : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
            W   : Natural := 64;
            H   : Natural := 64;
         begin
            if Image_Info_Maps.Has_Element (From.Page.Links.Pos) then
               From.Page.Links.Make_Image_Link
                 (Link   => Image_Info_Maps.Key (From.Page.Links.Pos),
                  Info   => Image_Info_Maps.Element (From.Page.Links.Pos),
                  URI    => URI,
                  Width  => W,
                  Height => H);
            end if;
            return Util.Beans.Objects.To_Object (URI);
         end;

      elsif Name = "imageTitle" then
         if Image_Info_Maps.Has_Element (From.Page.Links.Pos) then
            return Util.Beans.Objects.To_Object (Image_Info_Maps.Key (From.Page.Links.Pos));
         else
            return Util.Beans.Objects.Null_Object;
         end if;
      else
         return AWA.Wikis.Models.Wiki_Page_Info_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_Page_Info_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "wiki_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Wiki_Id := ADO.Utils.To_Identifier (Value);
         From.Page.Set_Wiki_Id (From.Wiki_Id);
      elsif Name = "page_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Page_Id := ADO.Utils.To_Identifier (Value);
         From.Page.Id := From.Page_Id;
      end if;

   exception
      when Constraint_Error =>
         From.Wiki_Id := ADO.NO_IDENTIFIER;
         From.Page_Id := ADO.NO_IDENTIFIER;

   end Set_Value;

   overriding
   procedure Load (Into    : in out Wiki_Page_Info_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  Load the wiki page first.
      Into.Page.Load (Outcome);
      if Outcome /= "loaded" then
         return;
      end if;
      declare
         use Ada.Strings.Unbounded;

         Doc      : Wiki.Documents.Document;
         Autolink : aliased Wiki.Filters.Autolink.Autolink_Filter;
         Images   : aliased Wiki.Filters.Collectors.Image_Collector_Type;
         Links    : aliased Wiki.Filters.Collectors.Link_Collector_Type;
         Words    : aliased Wiki.Filters.Collectors.Word_Collector_Type;
         Filter   : aliased Wiki.Filters.Html.Html_Filter_Type;
         Engine   : Wiki.Parsers.Parser;

         procedure Collect_Word (Pos : in Wiki.Filters.Collectors.Cursor);
         procedure Collect_Link (Pos : in Wiki.Filters.Collectors.Cursor);
         procedure Collect_Image (Pos : in Wiki.Filters.Collectors.Cursor);

         procedure Collect_Word (Pos : in Wiki.Filters.Collectors.Cursor) is
            Word  : constant Wiki.Strings.WString
              := Wiki.Filters.Collectors.WString_Maps.Key (Pos);
            Info  : AWA.Tags.Models.Tag_Info;
         begin
            Info.Count := Wiki.Filters.Collectors.WString_Maps.Element (Pos);
            Info.Tag := To_Unbounded_String (Wiki.Strings.To_String (Word));
            Into.Words.List.Append (Info);
         end Collect_Word;

         procedure Collect_Link (Pos : in Wiki.Filters.Collectors.Cursor) is
            Link  : constant Wiki.Strings.WString
              := Wiki.Filters.Collectors.WString_Maps.Key (Pos);
            Info  : AWA.Tags.Models.Tag_Info;
         begin
            Info.Count := Wiki.Filters.Collectors.WString_Maps.Element (Pos);
            Info.Tag := To_Unbounded_String (Wiki.Strings.To_String (Link));
            if Wiki.Helpers.Is_Url (Link) then
               Into.Ext_Links.List.Append (Info);
            else
               Into.Links.List.Append (Info);
            end if;
         end Collect_Link;

         procedure Collect_Image (Pos : in Wiki.Filters.Collectors.Cursor) is
            Image : constant Wiki.Strings.WString
              := Wiki.Filters.Collectors.WString_Maps.Key (Pos);
            URI   : Wiki.Strings.UString;
            W, H  : Natural := 0;
         begin
            Into.Page.Links.Make_Image_Link (Link   => Image,
                                             URI    => URI,
                                             Width  => W,
                                             Height => H);
         end Collect_Image;

         Content : constant String := To_String (Into.Page.Content.Value);
      begin
         Engine.Add_Filter (Words'Unchecked_Access);
         Engine.Add_Filter (Links'Unchecked_Access);
         Engine.Add_Filter (Images'Unchecked_Access);
         Engine.Add_Filter (Autolink'Unchecked_Access);
         Engine.Add_Filter (Filter'Unchecked_Access);
         Engine.Set_Syntax (Into.Page.Get_Syntax);
         Engine.Set_Plugin_Factory (Into.Page.Plugins'Access);
         Engine.Parse (Content, Doc);
         Words.Iterate (Collect_Word'Access);
         Links.Iterate (Collect_Link'Access);
         Images.Iterate (Collect_Image'Access);
      end;
   end Load;

   --  ------------------------------
   --  Create the Wiki_Page_Info_Bean bean instance.
   --  ------------------------------
   function Create_Wiki_Page_Info_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                        return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Wiki_Page_Info_Bean_Access := new Wiki_Page_Info_Bean;
   begin
      Object.Module         := Module;
      Object.Words_Bean     := Object.Words'Access;
      Object.Links_Bean     := Object.Links'Access;
      Object.Ext_Links_Bean := Object.Ext_Links'Access;
      Object.Page           := Get_Wiki_View_Bean ("wikiView");
      return Object.all'Access;
   end Create_Wiki_Page_Info_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wiki_Image_Info_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "wiki_id" then
         return ADO.Utils.To_Object (From.Wiki_Id);
      elsif Name = "page_id" then
         return ADO.Utils.To_Object (From.Page.Id);
      elsif Name = "folder_name" then
         return Util.Beans.Objects.To_Object (From.Folder_Name);
      elsif Name = "name" then
         return Util.Beans.Objects.To_Object (From.Name);
      elsif Name = "list" then
         return Util.Beans.Objects.To_Object (Value   => From.List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = "imageUrl" then
         declare
            Pos   : constant Natural := From.List_Bean.Get_Row_Index;
            Info  : AWA.Wikis.Models.Wiki_Image_Info;
            URI   : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
            W     : Natural := 0;
            H     : Natural := 0;
            Name  : constant String := Ada.Strings.Unbounded.To_String (From.Name);
            WName : constant Wiki.Strings.WString := Wiki.Strings.To_WString (Name);
         begin
            if Pos < From.List_Bean.Get_Count then
               Info := From.List_Bean.List.Element (Pos);
               if not Info.Width.Is_Null and not Info.Height.Is_Null then
                  W := Info.Width.Value;
                  H := Info.Height.Value;
               end if;
            end if;
            Info.Id     := From.Id;
            Info.Width  := From.Width;
            Info.Height := From.Height;
            From.Page.Links.Make_Image_Link (Link   => WName,
                                             Info   => Info,
                                             URI    => URI,
                                             Width  => W,
                                             Height => H);
            return Util.Beans.Objects.To_Object (URI);
         end;
      else
         return Models.Wiki_Image_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wiki_Image_Info_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "wiki_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Wiki_Id := ADO.NO_IDENTIFIER;
         From.Wiki_Id := ADO.Utils.To_Identifier (Value);
         From.Page.Set_Wiki_Id (From.Wiki_Id);
      elsif Name = "folder_name" then
         From.Folder_Name := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "name" then
         From.Name := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "page_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Page_Id := ADO.NO_IDENTIFIER;
         From.Page_Id := ADO.Utils.To_Identifier (Value);
         From.Page.Id := From.Page_Id;
      else
         Models.Wiki_Image_Bean (From).Set_Value (Name, Value);
      end if;

   exception
      when Constraint_Error =>
         null;
   end Set_Value;

   --  ------------------------------
   --  Load the information about the image.
   --  ------------------------------
   overriding
   procedure Load (Into    : in out Wiki_Image_Info_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type ADO.Identifier;
      use type Ada.Containers.Count_Type;
      use type Ada.Strings.Unbounded.Unbounded_String;

      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
   begin
      --  Load the wiki page first.
      Into.Page.Load (Outcome);
      if Outcome /= "loaded" then
         return;
      end if;

      --  Get the list of versions associated with the wiki page.
      Query.Set_Query (AWA.Wikis.Models.Query_Wiki_Image);
      Query.Bind_Param (Name => "file_name", Value => Into.Name);
      Query.Bind_Param (Name => "folder_name", Value => Into.Folder_Name);
      Query.Bind_Param (Name => "wiki_id", Value => Into.Wiki_Id);
      AWA.Wikis.Models.List (Into.List, Session, Query);
      if Into.List.List.Length > 0 then
         declare
            Img : constant AWA.Wikis.Models.Wiki_Image_Info := Into.List.List.First_Element;
         begin
            if Img.Id > 0 then
               Into.Id          := Img.Id;
               Into.Mime_Type   := Img.Mime_Type;
               Into.Storage     := Img.Storage;
               Into.File_Size   := Img.File_Size;
               Into.Create_Date := Img.Create_Date;
               Into.Width       := Img.Width;
               Into.Height      := Img.Height;
            end if;
            Into.Folder_Id   := Img.Folder_Id;
         end;
      end if;
   end Load;

   --  ------------------------------
   --  Create the Wiki_Image_Info_BEan bean instance.
   --  ------------------------------
   function Create_Wiki_Image_Info_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                         return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Wiki_Image_Info_Bean_Access := new Wiki_Image_Info_Bean;
   begin
      Object.Module         := Module;
      Object.List_Bean      := Object.List'Access;
      Object.Page           := Get_Wiki_View_Bean ("wikiView");
      Object.Id             := ADO.NO_IDENTIFIER;
      Object.Folder_Id      := ADO.NO_IDENTIFIER;
      Object.Width          := (Is_Null => True, Value => 0);
      Object.Height         := (Is_Null => True, Value => 0);
      return Object.all'Access;
   end Create_Wiki_Image_Info_Bean;

   --  ------------------------------
   --  Load the list of wikis.
   --  ------------------------------
   procedure Load_Wikis (List : in Wiki_Admin_Bean) is
      use AWA.Wikis.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
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
            return List.Wiki_List.List.Element (1).Id;
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
         From.Wiki_Id := ADO.NO_IDENTIFIER;
         From.Wiki_Id := ADO.Utils.To_Identifier (Value);
      end if;

   exception
      when Constraint_Error =>
         null;
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
