-----------------------------------------------------------------------
--  awa-components-wikis -- Wiki rendering component
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

with Util.Strings;
with Util.Beans.Objects;

with ASF.Contexts.Writer;
with ASF.Utils;

with AWA.Wikis.Writers;
package body AWA.Components.Wikis is

   WIKI_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  Get the wiki format style.  The format style is obtained from the <b>format</b>
   --  attribute name.
   --  ------------------------------
   function Get_Wiki_Style (UI      : in UIWiki;
                            Context : in Faces_Context'Class)
                            return AWA.Wikis.Parsers.Wiki_Syntax_Type is
      Format : constant String := UI.Get_Attribute (Name    => FORMAT_NAME,
                                                    Context => Context,
                                                    Default => "dotclear");
   begin
      if Format = "dotclear" then
         return AWA.Wikis.Parsers.SYNTAX_DOTCLEAR;
      elsif Format = "google" then
         return AWA.Wikis.Parsers.SYNTAX_GOOGLE;
      elsif Format = "phpbb" then
         return AWA.Wikis.Parsers.SYNTAX_PHPBB;
      elsif Format = "creole" then
         return AWA.Wikis.Parsers.SYNTAX_CREOLE;
      elsif Format = "mediawiki" then
         return AWA.Wikis.Parsers.SYNTAX_MEDIA_WIKI;
      else
         return AWA.Wikis.Parsers.SYNTAX_MIX;
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
         Html     : aliased AWA.Wikis.Writers.Html_Writer;
         Format   : constant AWA.Wikis.Parsers.Wiki_Syntax_Type := UI.Get_Wiki_Style (Context);
         Value    : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, VALUE_NAME);
      begin
         Writer.Start_Element ("div");
         UI.Render_Attributes (Context, WIKI_ATTRIBUTE_NAMES, Writer);

         if not Util.Beans.Objects.Is_Empty (Value) then
            Html.Set_Writer (Writer);
            AWA.Wikis.Parsers.Parse (Html'Unchecked_Access,
                                     Util.Beans.Objects.To_Wide_Wide_String (Value),
                                     Format);
         end if;
         Writer.End_Element ("div");
      end;
   end Encode_Begin;

begin
   ASF.Utils.Set_Text_Attributes (WIKI_ATTRIBUTE_NAMES);
end AWA.Components.Wikis;
