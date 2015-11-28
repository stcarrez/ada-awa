-----------------------------------------------------------------------
--  awa-components-wikis -- Wiki rendering component
--  Copyright (C) 2011, 2012, 2013, 2015 Stephane Carrez
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
with Util.Beans.Objects;

with Ada.Strings.Wide_Wide_Unbounded;
with ASF.Contexts.Writer;
with ASF.Utils;

with Wiki.Render.Html;
with Wiki.Writers;
package body AWA.Components.Wikis is

   use Ada.Strings.Wide_Wide_Unbounded;

   WIKI_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   type Html_Writer_Type is limited new Wiki.Writers.Html_Writer_Type with record
      Writer : ASF.Contexts.Writer.Response_Writer_Access;
   end record;
   type Html_Writer_Type_Access is access all Html_Writer_Type'Class;

   overriding
   procedure Write (Writer  : in out Html_Writer_Type;
                    Content : in Wide_Wide_String);

   --  Write a single character to the string builder.
   overriding
   procedure Write (Writer : in out Html_Writer_Type;
                    Char   : in Wide_Wide_Character);

   overriding
   procedure Write (Writer  : in out Html_Writer_Type;
                    Content : in Unbounded_Wide_Wide_String);


   --  Write an XML element using the given name and with the content.
   --  This is similar to calling <b>Start_Element</b>, <b>Write_Text</b>
   --  and <b>End_Element</b>.
   overriding
   procedure Write_Wide_Element (Writer  : in out Html_Writer_Type;
                                 Name    : in String;
                                 Content : in Unbounded_Wide_Wide_String);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   overriding
   procedure Write_Wide_Attribute (Writer  : in out Html_Writer_Type;
                                   Name    : in String;
                                   Content : in Unbounded_Wide_Wide_String);

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
                              Content : in Unbounded_Wide_Wide_String);

   overriding
   procedure Write (Writer  : in out Html_Writer_Type;
                    Content : in Wide_Wide_String) is
   begin
      Writer.Writer.Write_Wide_Text (Content);
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

   overriding
   procedure Write (Writer  : in out Html_Writer_Type;
                    Content : in Unbounded_Wide_Wide_String) is
   begin
      Writer.Writer.Write (Content);
   end Write;


   --  ------------------------------
   --  Write an XML element using the given name and with the content.
   --  This is similar to calling <b>Start_Element</b>, <b>Write_Text</b>
   --  and <b>End_Element</b>.
   --  ------------------------------
   overriding
   procedure Write_Wide_Element (Writer  : in out Html_Writer_Type;
                                 Name    : in String;
                                 Content : in Unbounded_Wide_Wide_String) is
   begin
      Writer.Writer.Write_Wide_Element (Name, Content);
   end Write_Wide_Element;

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
                              Content : in Unbounded_Wide_Wide_String) is
   begin
      Writer.Writer.Write_Wide_Text (Content);
   end Write_Wide_Text;

   --  ------------------------------
   --  Get the wiki format style.  The format style is obtained from the <b>format</b>
   --  attribute name.
   --  ------------------------------
   function Get_Wiki_Style (UI      : in UIWiki;
                            Context : in Faces_Context'Class)
                            return Wiki.Parsers.Wiki_Syntax_Type is
      Format : constant String := UI.Get_Attribute (Name    => FORMAT_NAME,
                                                    Context => Context,
                                                    Default => "dotclear");
   begin
      if Format = "dotclear" then
         return Wiki.Parsers.SYNTAX_DOTCLEAR;
      elsif Format = "google" then
         return Wiki.Parsers.SYNTAX_GOOGLE;
      elsif Format = "phpbb" then
         return Wiki.Parsers.SYNTAX_PHPBB;
      elsif Format = "creole" then
         return Wiki.Parsers.SYNTAX_CREOLE;
      elsif Format = "mediawiki" then
         return Wiki.Parsers.SYNTAX_MEDIA_WIKI;
      else
         return Wiki.Parsers.SYNTAX_MIX;
      end if;
   end Get_Wiki_Style;

   --  ------------------------------
   --  Render the wiki text
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIWiki;
                           Context : in out Faces_Context'Class) is
      use ASF.Contexts.Writer;
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Html     : aliased Html_Writer_Type;
         Renderer : aliased Wiki.Render.Html.Html_Renderer;
         Format   : constant Wiki.Parsers.Wiki_Syntax_Type := UI.Get_Wiki_Style (Context);
         Value    : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, VALUE_NAME);
      begin
         Html.Writer := Writer;
         Writer.Start_Element ("div");
         UI.Render_Attributes (Context, WIKI_ATTRIBUTE_NAMES, Writer);

         if not Util.Beans.Objects.Is_Empty (Value) then
            Renderer.Set_Writer (Html'Unchecked_Access);
            Wiki.Parsers.Parse (Renderer'Unchecked_Access,
                                Util.Beans.Objects.To_Wide_Wide_String (Value),
                                Format);
         end if;
         Writer.End_Element ("div");
      end;
   end Encode_Begin;

begin
   ASF.Utils.Set_Text_Attributes (WIKI_ATTRIBUTE_NAMES);
end AWA.Components.Wikis;
