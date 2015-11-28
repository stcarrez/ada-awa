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

with ASF.Contexts.Faces;
with ASF.Components;
with ASF.Components.Html;

with Wiki.Parsers;
package AWA.Components.Wikis is

   use ASF.Contexts.Faces;

   --  The wiki format of the wiki text.  The valid values are:
   --  dotclear, google, creole, phpbb, mediawiki
   FORMAT_NAME : constant String := "format";

   VALUE_NAME  : constant String := ASF.Components.VALUE_NAME;

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
                            return Wiki.Parsers.Wiki_Syntax_Type;

   --  Render the wiki text
   overriding
   procedure Encode_Begin (UI      : in UIWiki;
                           Context : in out Faces_Context'Class);

end AWA.Components.Wikis;
