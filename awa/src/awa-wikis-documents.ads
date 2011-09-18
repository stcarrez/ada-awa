-----------------------------------------------------------------------
--  awa-wikis-documents -- Wiki module
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
with Ada.Strings.Wide_Wide_Unbounded;

package AWA.Wikis.Documents is

   pragma Preelaborate;

   use Ada.Strings.Wide_Wide_Unbounded;

   type Format_Type is (BOLD, ITALIC, CODE, SUPERSCRIPT, SUBSCRIPT, STRIKEOUT, PREFORMAT);

   type Format_Map is array (Format_Type) of Boolean;

   --  ------------------------------
   --  Document reader
   --  ------------------------------
   type Document_Reader is limited interface;
   type Document_Reader_Access is access all Document_Reader'Class;

   --  Add a section header in the document.
   procedure Add_Header (Document : in out Document_Reader;
                         Header   : in Unbounded_Wide_Wide_String;
                         Level    : in Positive) is abstract;

   --  Add a line break (<br>).
   procedure Add_Line_Break (Document : in out Document_Reader) is abstract;

   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   procedure Add_Paragraph (Document : in out Document_Reader) is abstract;

   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   procedure Add_List_Item (Document : in out Document_Reader;
                            Level    : in Positive;
                            Ordered  : in Boolean) is abstract;

   --  Add an horizontal rule (<hr>).
   procedure Add_Horizontal_Rule (Document : in out Document_Reader) is abstract;

   --  Add a link.
   procedure Add_Link (Document : in out Document_Reader;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String) is abstract;

   --  Add an image.
   procedure Add_Image (Document    : in out Document_Reader;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String) is abstract;

   --  Add a quote.
   procedure Add_Quote (Document : in out Document_Reader;
                        Quote    : in Unbounded_Wide_Wide_String;
                        Link     : in Unbounded_Wide_Wide_String;
                        Language : in Unbounded_Wide_Wide_String) is abstract;

   --  Add a text block with the given format.
   procedure Add_Text (Document : in out Document_Reader;
                       Text     : in Unbounded_Wide_Wide_String;
                       Format   : in Format_Map) is abstract;

   --  Add a text block that is pre-formatted.
   procedure Add_Preformatted (Document : in out Document_Reader;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String) is abstract;

   --  Finish the document after complete wiki text has been parsed.
   procedure Finish (Document : in out Document_Reader) is abstract;

end AWA.Wikis.Documents;
