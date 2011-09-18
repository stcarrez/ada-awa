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

with AWA.Wikis.Parsers;
with AWA.Wikis.Writers;
package body AWA.Components.Wikis is

   WIKI_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

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
         Value    : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "value");
      begin
         Writer.Start_Element ("div");
         UI.Render_Attributes (Context, WIKI_ATTRIBUTE_NAMES, Writer);

         if not Util.Beans.Objects.Is_Empty (Value) then
            Html.Set_Writer (Writer);
            AWA.Wikis.Parsers.Parse (Html'Unchecked_Access,
                                     Util.Beans.Objects.To_Wide_Wide_String (Value));
         end if;
         Writer.End_Element ("div");
      end;
   end Encode_Begin;

begin
   ASF.Utils.Set_Text_Attributes (WIKI_ATTRIBUTE_NAMES);
end AWA.Components.Wikis;
