-----------------------------------------------------------------------
--  awa-components-wikis -- Wiki rendering component
--  Copyright (C) 2011 - 2025 Stephane Carrez
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
with Util.Strings;

with Ada.Characters.Conversions;
with ASF.Contexts.Writer;
with ASF.Utils;

with Wiki.Documents;
with Wiki.Parsers;
with Wiki.Helpers;
with Wiki.Render.Html;
with Wiki.Render.Text;
with Wiki.Filters.Html;
with Wiki.Filters.TOC;
with Wiki.Filters.Autolink;
with Wiki.Filters.Variables;
with Wiki.Streams.Html;
package body AWA.Components.Wikis is

   WIKI_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   type Html_Writer_Type is limited new Wiki.Streams.Html.Html_Output_Stream with record
      Writer : ASF.Contexts.Writer.Response_Writer_Access;
   end record;

   --  Enable/disable strict XML generation.  When disabled, the <br>, <hr>,
   --  <img> elements are not closed.
   overriding
   procedure Set_Strict_XML (Writer : in out Html_Writer_Type;
                             Strict : in Boolean) is null;

   --  Enable/disable indentation temporarily.
   overriding
   procedure Set_Enable_Indent (Writer : in out Html_Writer_Type;
                                Enable : in Boolean) is null;

   overriding
   procedure Write (Writer  : in out Html_Writer_Type;
                    Content : in Wide_Wide_String);

   --  Write a single character to the string builder.
   overriding
   procedure Write (Writer : in out Html_Writer_Type;
                    Char   : in Wide_Wide_Character);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   overriding
   procedure Write_Wide_Attribute (Writer  : in out Html_Writer_Type;
                                   Name    : in String;
                                   Content : in Unbounded_Wide_Wide_String);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   overriding
   procedure Write_Wide_Attribute (Writer  : in out Html_Writer_Type;
                                   Name    : in String;
                                   Content : in Wiki.Strings.WString);

   --  Start an XML element with the given name.
   overriding
   procedure Start_Element (Writer : in out Html_Writer_Type;
                            Name   : in String);

   --  Closes an XML element of the given name.
   overriding
   procedure End_Element (Writer : in out Html_Writer_Type;
                          Name   : in String);

   --  Write a text escaping any character as necessary.
   overriding
   procedure Write_Wide_Text (Writer  : in out Html_Writer_Type;
                              Content : in Wiki.Strings.WString);

   overriding
   procedure Write (Writer  : in out Html_Writer_Type;
                    Content : in Wiki.Strings.WString) is
   begin
      Writer.Writer.Write_Wide_Raw (Content);
   end Write;

   --  ------------------------------
   --  Write a single character to the string builder.
   --  ------------------------------
   overriding
   procedure Write (Writer : in out Html_Writer_Type;
                    Char   : in Wide_Wide_Character) is
   begin
      Writer.Writer.Write_Wide_Char (Char);
   end Write;

   --  ------------------------------
   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   --  ------------------------------
   overriding
   procedure Write_Wide_Attribute (Writer  : in out Html_Writer_Type;
                                   Name    : in String;
                                   Content : in Unbounded_Wide_Wide_String) is
   begin
      Writer.Writer.Write_Wide_Attribute (Name, Content);
   end Write_Wide_Attribute;

   --  ------------------------------
   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   --  ------------------------------
   overriding
   procedure Write_Wide_Attribute (Writer  : in out Html_Writer_Type;
                                   Name    : in String;
                                   Content : in Wiki.Strings.WString) is
   begin
      Writer.Writer.Write_Wide_Attribute (Name, Content);
   end Write_Wide_Attribute;

   --  ------------------------------
   --  Start an XML element with the given name.
   --  ------------------------------
   overriding
   procedure Start_Element (Writer : in out Html_Writer_Type;
                            Name   : in String) is
   begin
      Writer.Writer.Start_Element (Name);
   end Start_Element;

   --  ------------------------------
   --  Closes an XML element of the given name.
   --  ------------------------------
   overriding
   procedure End_Element (Writer : in out Html_Writer_Type;
                          Name   : in String) is
   begin
      Writer.Writer.End_Element (Name);
   end End_Element;

   --  ------------------------------
   --  Write a text escaping any character as necessary.
   --  ------------------------------
   overriding
   procedure Write_Wide_Text (Writer  : in out Html_Writer_Type;
                              Content : in Wiki.Strings.WString) is
   begin
      Writer.Writer.Write_Wide_Text (Content);
   end Write_Wide_Text;

   --  ------------------------------
   --  Get the wiki format style.  The format style is obtained from the <b>format</b>
   --  attribute name.
   --  ------------------------------
   function Get_Wiki_Style (UI      : in UIWiki;
                            Context : in Faces_Context'Class)
                            return Wiki.Wiki_Syntax is
      Format : constant String := UI.Get_Attribute (Name    => FORMAT_NAME,
                                                    Context => Context,
                                                    Default => "dotclear");
   begin
      if Format in "dotclear" | "FORMAT_DOTCLEAR" then
         return Wiki.SYNTAX_DOTCLEAR;
      elsif Format = "google" then
         return Wiki.SYNTAX_GOOGLE;
      elsif Format in "phpbb" | "FORMAT_PHPBB" then
         return Wiki.SYNTAX_PHPBB;
      elsif Format in "creole" | "FORMAT_CREOLE" then
         return Wiki.SYNTAX_CREOLE;
      elsif Format in "markdown" | "FORMAT_MARKDOWN" then
         return Wiki.SYNTAX_MARKDOWN;
      elsif Format in "mediawiki" | "FORMAT_MEDIAWIKI" then
         return Wiki.SYNTAX_MEDIA_WIKI;
      elsif Format in "html" | "FORMAT_HTML" then
         return Wiki.SYNTAX_HTML;
      else
         return Wiki.SYNTAX_MARKDOWN;
      end if;
   end Get_Wiki_Style;

   --  ------------------------------
   --  Get the links renderer that must be used to render image and page links.
   --  ------------------------------
   function Get_Links_Renderer (UI      : in UIWiki;
                                Context : in Faces_Context'Class)
                                return Wiki.Render.Links.Link_Renderer_Access is
      Value : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, LINKS_NAME);
      Bean  : constant access Util.Beans.Basic.Readonly_Bean'Class
           := Util.Beans.Objects.To_Bean (Value);
   begin
      if Bean = null then
         return null;
      elsif not (Bean.all in Link_Renderer_Bean'Class) then
         return null;
      else
         return Link_Renderer_Bean'Class (Bean.all)'Access;
      end if;
   end Get_Links_Renderer;

   --  ------------------------------
   --  Get the plugin factory that must be used by the Wiki parser.
   --  ------------------------------
   function Get_Plugin_Factory (UI      : in UIWiki;
                                Context : in Faces_Context'Class)
                                return Wiki.Plugins.Plugin_Factory_Access is
      Value : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, PLUGINS_NAME);
      Bean  : constant access Util.Beans.Basic.Readonly_Bean'Class
           := Util.Beans.Objects.To_Bean (Value);
   begin
      if Bean = null then
         return null;
      elsif not (Bean.all in Wiki.Plugins.Plugin_Factory'Class) then
         return null;
      else
         return Wiki.Plugins.Plugin_Factory'Class (Bean.all)'Access;
      end if;
   end Get_Plugin_Factory;

   --  ------------------------------
   --  Returns true if we must render a text content instead of html.
   --  ------------------------------
   function Is_Text (UI      : in UIWiki;
                     Context : in Faces_Context'Class) return Boolean is
      use type Util.Beans.Objects.Object;

      Output : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, OUTPUT_NAME);
   begin
      if Util.Beans.Objects.Is_Null (Output) then
         return False;
      end if;
      return Output = Util.Beans.Objects.To_Object (String '("text"));
   end Is_Text;

   --  ------------------------------
   --  Render the wiki text
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIWiki;
                           Context : in out Faces_Context'Class) is
      use ASF.Contexts.Writer;
      use type Wiki.Render.Links.Link_Renderer_Access;
      use type Wiki.Plugins.Plugin_Factory_Access;
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Doc      : Wiki.Documents.Document;
         TOC      : aliased Wiki.Filters.TOC.TOC_Filter;
         Autolink : aliased Wiki.Filters.Autolink.Autolink_Filter;
         Filter   : aliased Wiki.Filters.Html.Html_Filter_Type;
         Vars     : aliased Wiki.Filters.Variables.Variable_Filter;
         Format   : constant Wiki.Wiki_Syntax := UI.Get_Wiki_Style (Context);
         Value    : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, VALUE_NAME);
         Is_Text  : constant Boolean := UI.Is_Text (Context);
         Links    : Wiki.Render.Links.Link_Renderer_Access;
         Plugins  : Wiki.Plugins.Plugin_Factory_Access;
         Engine   : Wiki.Parsers.Parser;
      begin
         if not Is_Text then
            Writer.Start_Element ("div");
            UI.Render_Attributes (Context, WIKI_ATTRIBUTE_NAMES, Writer);
         end if;

         if not Util.Beans.Objects.Is_Empty (Value) then
            Plugins := UI.Get_Plugin_Factory (Context);
            if Plugins /= null then
               Engine.Set_Plugin_Factory (Plugins);
            end if;
            Links := UI.Get_Links_Renderer (Context);
            Engine.Add_Filter (Autolink'Unchecked_Access);
            Engine.Add_Filter (Vars'Unchecked_Access);
            Engine.Add_Filter (TOC'Unchecked_Access);
            Engine.Add_Filter (Filter'Unchecked_Access);
            Engine.Set_Syntax (Format);
            Engine.Parse (Util.Beans.Objects.To_String (Value), Doc);
            if not Is_Text then
               declare
                  Html     : aliased Html_Writer_Type;
                  Renderer : aliased Wiki.Render.Html.Html_Renderer;
               begin
                  Html.Writer := Writer;
                  if Links /= null then
                     Renderer.Set_Link_Renderer (Links);
                  end if;
                  Renderer.Set_Output_Stream (Html'Unchecked_Access);
                  Renderer.Set_Render_TOC (UI.Get_Attribute (TOC_NAME, Context, False));
                  Renderer.Render (Doc);
               end;
            else
               declare
                  Html     : aliased Html_Writer_Type;
                  Renderer : aliased Wiki.Render.Text.Text_Renderer;
               begin
                  Html.Writer := Writer;
                  Renderer.Set_Output_Stream (Html'Unchecked_Access);
                  Renderer.Render (Doc);
               end;
            end if;
         end if;
         if not Is_Text then
            Writer.End_Element ("div");
         end if;
      end;
   end Encode_Begin;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Link_Renderer_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = IMAGE_PREFIX_ATTR then
         return Util.Beans.Objects.To_Object (From.Image_Prefix);
      elsif Name = PAGE_PREFIX_ATTR then
         return Util.Beans.Objects.To_Object (From.Page_Prefix);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Link_Renderer_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = IMAGE_PREFIX_ATTR then
         From.Image_Prefix := Util.Beans.Objects.To_Unbounded_Wide_Wide_String (Value);
      elsif Name = PAGE_PREFIX_ATTR then
         From.Page_Prefix := Util.Beans.Objects.To_Unbounded_Wide_Wide_String (Value);
      end if;
   end Set_Value;

   function Starts_With (Content : in Unbounded_Wide_Wide_String;
                         Item    : in String) return Boolean is
      use Ada.Characters.Conversions;

      Pos : Positive := 1;
   begin
      if Length (Content) < Item'Length then
         return False;
      end if;
      for I in Item'Range loop
         if Item (I) /= To_Character (Element (Content, Pos)) then
            return False;
         end if;
         Pos := Pos + 1;
      end loop;
      return True;
   end Starts_With;

   procedure Make_Link (Renderer : in Link_Renderer_Bean;
                        Link     : in Wiki.Strings.WString;
                        Prefix   : in Unbounded_Wide_Wide_String;
                        URI      : out Unbounded_Wide_Wide_String) is
      pragma Unreferenced (Renderer);
   begin
      if Wiki.Helpers.Is_Url (Link) then
         URI := To_Unbounded_Wide_Wide_String (Link);
      else
         URI := Prefix & Link;
      end if;
   end Make_Link;

   --  ------------------------------
   --  Get the image link that must be rendered from the wiki image link.
   --  ------------------------------
   overriding
   procedure Make_Image_Link (Renderer : in out Link_Renderer_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural) is
   begin
      Renderer.Make_Link (Link, Renderer.Image_Prefix, URI);
      Width  := 0;
      Height := 0;
   end Make_Image_Link;

   --  ------------------------------
   --  Get the page link that must be rendered from the wiki page link.
   --  ------------------------------
   overriding
   procedure Make_Page_Link (Renderer : in out Link_Renderer_Bean;
                             Link     : in Wiki.Strings.WString;
                             URI      : out Unbounded_Wide_Wide_String;
                             Exists   : out Boolean) is
   begin
      Renderer.Make_Link (Link, Renderer.Page_Prefix, URI);
      Exists := True;
   end Make_Page_Link;

begin
   ASF.Utils.Set_Text_Attributes (WIKI_ATTRIBUTE_NAMES);
end AWA.Components.Wikis;
