-----------------------------------------------------------------------
--  awa-components-wikis -- Wiki rendering component
--  Copyright (C) 2011, 2015, 2016, 2021 Stephane Carrez
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
with Ada.Strings.Wide_Wide_Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;

with ASF.Contexts.Faces;
with ASF.Components;
with ASF.Components.Html;

with Wiki.Strings;
with Wiki.Render;
with Wiki.Plugins;
with Wiki.Render.Links;
package AWA.Components.Wikis is

   use ASF.Contexts.Faces;

   --  The wiki format of the wiki text.  The valid values are:
   --  dotclear, markdown, creole, mediawiki, html
   FORMAT_NAME : constant String := "format";

   VALUE_NAME  : constant String := ASF.Components.VALUE_NAME;

   --  The link renderer bean that controls the generation of page and image links.
   LINKS_NAME  : constant String := "links";

   --  The plugin factory bean that must be used for Wiki plugins.
   PLUGINS_NAME  : constant String := "plugins";

   --  Whether the TOC is rendered in the document.
   TOC_NAME      : constant String := "toc";

   --  The output format when rendering the content ("text" or "html", default is "html").
   OUTPUT_NAME   : constant String := "output";

   --  ------------------------------
   --  Wiki component
   --  ------------------------------
   --
   --  <awa:wiki value="wiki-text" format="dotclear|google|creole|phpbb" styleClass="class"/>
   --
   type UIWiki is new ASF.Components.Html.UIHtmlComponent with null record;
   type UIWiki_Access is access all UIWiki'Class;

   --  Get the wiki format style.  The format style is obtained from the <b>format</b>
   --  attribute name.
   function Get_Wiki_Style (UI      : in UIWiki;
                            Context : in Faces_Context'Class)
                            return Wiki.Wiki_Syntax;

   --  Get the links renderer that must be used to render image and page links.
   function Get_Links_Renderer (UI      : in UIWiki;
                                Context : in Faces_Context'Class)
                                return Wiki.Render.Links.Link_Renderer_Access;

   --  Get the plugin factory that must be used by the Wiki parser.
   function Get_Plugin_Factory (UI      : in UIWiki;
                                Context : in Faces_Context'Class)
                                return Wiki.Plugins.Plugin_Factory_Access;

   --  Returns true if we must render a text content instead of html.
   function Is_Text (UI      : in UIWiki;
                     Context : in Faces_Context'Class) return Boolean;

   --  Render the wiki text
   overriding
   procedure Encode_Begin (UI      : in UIWiki;
                           Context : in out Faces_Context'Class);

   use Ada.Strings.Wide_Wide_Unbounded;

   IMAGE_PREFIX_ATTR : constant String := "image_prefix";
   PAGE_PREFIX_ATTR  : constant String := "page_prefix";

   type Link_Renderer_Bean is new Util.Beans.Basic.Bean
     and Wiki.Render.Links.Link_Renderer with record
      Page_Prefix  : Unbounded_Wide_Wide_String;
      Image_Prefix : Unbounded_Wide_Wide_String;
   end record;

   --  Make a link adding a prefix unless the link is already absolute.
   procedure Make_Link (Renderer : in Link_Renderer_Bean;
                        Link     : in Wiki.Strings.WString;
                        Prefix   : in Unbounded_Wide_Wide_String;
                        URI      : out Unbounded_Wide_Wide_String);

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Link_Renderer_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Link_Renderer_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Get the image link that must be rendered from the wiki image link.
   overriding
   procedure Make_Image_Link (Renderer : in out Link_Renderer_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural);

   --  Get the page link that must be rendered from the wiki page link.
   overriding
   procedure Make_Page_Link (Renderer : in out Link_Renderer_Bean;
                             Link     : in Wiki.Strings.WString;
                             URI      : out Unbounded_Wide_Wide_String;
                             Exists   : out Boolean);

private

   function Starts_With (Content : in Unbounded_Wide_Wide_String;
                         Item    : in String) return Boolean;

end AWA.Components.Wikis;
