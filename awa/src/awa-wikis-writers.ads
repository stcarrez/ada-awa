-----------------------------------------------------------------------
--  awa-wikis-writers -- Wiki writers
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
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

with AWA.Wikis.Parsers;
package AWA.Wikis.Writers is

   --  Render the wiki text according to the wiki syntax in an HTML string.
   function To_Html (Text   : in Wide_Wide_String;
                     Syntax : in AWA.Wikis.Parsers.Wiki_Syntax_Type) return String;

   --  Render the wiki text according to the wiki syntax in a text string.
   --  Wiki formatting and decoration are removed.
   function To_Text (Text   : in Wide_Wide_String;
                     Syntax : in AWA.Wikis.Parsers.Wiki_Syntax_Type) return String;

end AWA.Wikis.Writers;
