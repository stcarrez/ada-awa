-----------------------------------------------------------------------
--  awa-wikis-writers-text -- Wiki Text writer
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
with Ada.Strings.Wide_Wide_Unbounded;

with AWA.Wikis.Documents;
with ASF.Contexts.Writer;
package AWA.Wikis.Writers.Text is

   use Ada.Strings.Wide_Wide_Unbounded;

   --  ------------------------------
   --  Wiki to Text writer
   --  ------------------------------
   type Text_Writer is new AWA.Wikis.Documents.Document_Reader with private;

   --  Set the output writer.
   procedure Set_Writer (Document : in out Text_Writer;
                         Writer   : in ASF.Contexts.Writer.Response_Writer_Access);

   --  Add a section header in the document.
   overriding
   procedure Add_Header (Document : in out Text_Writer;
                         Header   : in Unbounded_Wide_Wide_String;
                         Level    : in Positive);

   --  Add a line break (<br>).
   overriding
   procedure Add_Line_Break (Document : in out Text_Writer);

   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   overriding
   procedure Add_Paragraph (Document : in out Text_Writer);

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   overriding
   procedure Add_Blockquote (Document : in out Text_Writer;
                             Level    : in Natural);

   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   overriding
   procedure Add_List_Item (Document : in out Text_Writer;
                            Level    : in Positive;
                            Ordered  : in Boolean);

   --  Add an horizontal rule (<hr>).
   overriding
   procedure Add_Horizontal_Rule (Document : in out Text_Writer);

   --  Add a link.
   overriding
   procedure Add_Link (Document : in out Text_Writer;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String);

   --  Add an image.
   overriding
   procedure Add_Image (Document    : in out Text_Writer;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String);

   --  Add a quote.
   overriding
   procedure Add_Quote (Document : in out Text_Writer;
                        Quote    : in Unbounded_Wide_Wide_String;
                        Link     : in Unbounded_Wide_Wide_String;
                        Language : in Unbounded_Wide_Wide_String);

   --  Add a text block with the given format.
   overriding
   procedure Add_Text (Document : in out Text_Writer;
                       Text     : in Unbounded_Wide_Wide_String;
                       Format   : in AWA.Wikis.Documents.Format_Map);

   --  Add a text block that is pre-formatted.
   procedure Add_Preformatted (Document : in out Text_Writer;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String);

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Document : in out Text_Writer);

private

   procedure Close_Paragraph (Document : in out Text_Writer);
   procedure Open_Paragraph (Document : in out Text_Writer);

   type Text_Writer is new AWA.Wikis.Documents.Document_Reader with record
      Writer         : ASF.Contexts.Writer.Response_Writer_Access := null;
      Format         : AWA.Wikis.Documents.Format_Map := (others => False);
      Has_Paragraph  : Boolean := False;
      Need_Paragraph : Boolean := False;
      Empty_Line     : Boolean := True;
   end record;

end AWA.Wikis.Writers.Text;
