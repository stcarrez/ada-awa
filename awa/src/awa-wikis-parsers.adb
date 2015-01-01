-----------------------------------------------------------------------
--  awa-wikis-parsers -- Wiki parser
--  Copyright (C) 2011, 2012, 2013, 2014, 2015 Stephane Carrez
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

package body AWA.Wikis.Parsers is

   use AWA.Wikis.Documents;
   use Ada.Strings.Wide_Wide_Unbounded;

   --  Peek the next character from the wiki text buffer.
   procedure Peek (P     : in out Parser;
                   Token : out Wide_Wide_Character);

   --  Put back the character so that it will be returned by the next call to Peek.
   procedure Put_Back (P     : in out Parser;
                       Token : in Wide_Wide_Character);

   --  Flush the wiki text that was collected in the text buffer.
   procedure Flush_Text (P : in out Parser);

   --  Append a character to the wiki text buffer.
   procedure Parse_Text (P     : in out Parser;
                         Token : in Wide_Wide_Character);

   --  Parse the beginning or the end of a double character sequence.  This procedure
   --  is instantiated for several format types (bold, italic, superscript, subscript, code).
   --  Example:
   --    --name--  **bold** ~~strike~~
   generic
      Format : Format_Type;
   procedure Parse_Double_Format (P     : in out Parser;
                                  Token : in Wide_Wide_Character);

   --  Parse the beginning or the end of a single character sequence.  This procedure
   --  is instantiated for several format types (bold, italic, superscript, subscript, code).
   --  Example:
   --    _name_    *bold*   `code`
   generic
      Format : Format_Type;
   procedure Parse_Single_Format (P     : in out Parser;
                                  Token : in Wide_Wide_Character);

   --  Parse an italic, bold or bold + italic sequence.
   --  Example:
   --    ''name''         (italic)
   --    '''name'''       (bold)
   --    '''''name'''''   (bold+italic)
   procedure Parse_Bold_Italic (P     : in out Parser;
                                Token : in Wide_Wide_Character);

   --  Parse a line break.
   --  Example:
   --     \\    (Creole)
   --     %%%   (Dotclear)
   procedure Parse_Line_Break (P     : in out Parser;
                               Token : in Wide_Wide_Character);

   --  Parse a link.
   --  Example:
   --    [name]
   --    [name|url]
   --    [name|url|language]
   --    [name|url|language|title]
   --    [[link]]
   --    [[link|name]]
   --  ------------------------------
   procedure Parse_Link (P     : in out Parser;
                         Token : in Wide_Wide_Character);

   --  Parse a space and take necessary formatting actions.
   --  Example:
   --    item1 item2   => add space in text buffer
   --    ' * item'     => start a bullet list (Google)
   --    ' # item'     => start an ordered list (Google)
   --    ' item'       => preformatted text (Google, Creole)
   procedure Parse_Space (P     : in out Parser;
                          Token : in Wide_Wide_Character);

   --  Parse a wiki heading.  The heading could start with '=' or '!'.
   --  The trailing equals are ignored.
   --  Example:
   --    == Level 2 ==
   --    !!! Level 3
   procedure Parse_Header (P     : in out Parser;
                           Token : in Wide_Wide_Character);

   --  Parse an image.
   --  Example:
   --    ((url|alt text))
   --    ((url|alt text|position))
   --    ((url|alt text|position||description))
   procedure Parse_Image (P     : in out Parser;
                          Token : in Wide_Wide_Character);

   --  Parse a quote.
   --  Example:
   --    {{name}}
   --    {{name|language}}
   --    {{name|language|url}}
   procedure Parse_Quote (P     : in out Parser;
                          Token : in Wide_Wide_Character);

   procedure Parse_Token (P     : in out Parser;
                          Table : in Parser_Table);

   procedure Parse_End_Line (P     : in out Parser;
                             Token : in Wide_Wide_Character);

   procedure Parse_Preformatted (P     : in out Parser;
                                 Token : in Wide_Wide_Character);

   --  Parse a blockquote.
   --  Example:
   --    >>>quote level 3
   --    >>quote level 2
   --    >quote level 1
   procedure Parse_Blockquote (P     : in out Parser;
                               Token : in Wide_Wide_Character);

   procedure Parse_List (P     : in out Parser;
                         Token : in Wide_Wide_Character);

   procedure Toggle_Format (P      : in out Parser;
                            Format : in Format_Type);

   --  ------------------------------
   --  Peek the next character from the wiki text buffer.
   --  ------------------------------
   procedure Peek (P     : in out Parser;
                   Token : out Wide_Wide_Character) is
   begin
      if P.Has_Pending then
         --  Return the pending character.
         Token         := P.Pending;
         P.Has_Pending := False;

      elsif P.Is_Eof then
         --  Return a \n on end of file (this simplifies the implementation).
         Token := LF;
      else

         --  Get the next character.
         P.Reader.Read_Char (Token, P.Is_Eof);
         if P.Is_Eof then
            Token := LF;
         end if;
      end if;
   end Peek;

   --  ------------------------------
   --  Put back the character so that it will be returned by the next call to Peek.
   --  ------------------------------
   procedure Put_Back (P     : in out Parser;
                       Token : in Wide_Wide_Character) is
   begin
      P.Pending     := Token;
      P.Has_Pending := True;
   end Put_Back;

   --  ------------------------------
   --  Flush the wiki text that was collected in the text buffer.
   --  ------------------------------
   procedure Flush_Text (P : in out Parser) is
   begin
      if Length (P.Text) > 0 then
         P.Document.Add_Text (P.Text, P.Format);
         P.Text := Null_Unbounded_Wide_Wide_String;
      end if;
   end Flush_Text;

   --  ------------------------------
   --  Append a character to the wiki text buffer.
   --  ------------------------------
   procedure Parse_Text (P     : in out Parser;
                         Token : in Wide_Wide_Character) is
   begin
      Append (P.Text, Token);
      P.Empty_Line := False;
   end Parse_Text;

   --  ------------------------------
   --  Parse a pre-formatted text which starts either by a space or by a sequence
   --  of characters.  Example:
   --    {{{
   --    pre-formatted
   --    }}}
   --    ' pre-formattted'
   --  ------------------------------
   procedure Parse_Preformatted (P     : in out Parser;
                                 Token : in Wide_Wide_Character) is
      C          : Wide_Wide_Character;
      Stop_Token : Wide_Wide_Character;
      Format     : Unbounded_Wide_Wide_String;
      Col        : Natural;
   begin
      if Token /= ' ' then
         Peek (P, C);
         if C /= Token then
            Parse_Text (P, Token);
            Put_Back (P, C);
            return;
         end if;
         Peek (P, C);
         if C /= Token then
            Parse_Text (P, Token);
            Parse_Text (P, Token);
            Put_Back (P, C);
            return;
         end if;
      elsif not P.Is_Dotclear or else not P.Empty_Line then
         Parse_Text (P, Token);
         return;
      end if;
      Flush_Text (P);
      if Token = ' ' then
         Col := 1;
         while not P.Is_Eof loop
            Peek (P, C);
            if Col = 0 then
               if C /= ' ' then
                  Put_Back (P, C);
                  exit;
               end if;
               Col := Col + 1;
            elsif C = LF or C = CR then
               Col := 0;
               Append (P.Text, C);
            else
               Col := Col + 1;
               Append (P.Text, C);
            end if;
         end loop;
      else
         Peek (P, C);
         if Token = '{' then
            if C /= LF and C /= CR then
               Put_Back (P, C);
               P.Format (CODE) := True;
               return;
            end if;
         elsif Token = '}' then
            Put_Back (P, C);
            P.Format (CODE) := True;
            return;
         elsif Token /= ' ' then
            while not P.Is_Eof and C /= LF and C /= CR loop
               Append (Format, C);
               Peek (P, C);
            end loop;
         end if;
         if Token = '{' then
            Stop_Token := '}';
         else
            Stop_Token := Token;
         end if;
         Col := 0;
         while not P.Is_Eof loop
            Peek (P, C);
            if Stop_Token = C and Col = 0 then
               Peek (P, C);
               if C = Stop_Token then
                  Peek (P, C);
                  exit when C = Stop_Token;
               end if;
               Append (P.Text, Stop_Token);
               Col := Col + 1;
            elsif C = LF or C = CR then
               Col := 0;
            else
               Col := Col + 1;
            end if;
            Append (P.Text, C);
         end loop;
      end if;
      P.Empty_Line := True;

      P.Document.Add_Preformatted (P.Text, Format);
      P.Text := Null_Unbounded_Wide_Wide_String;
      P.Document.Add_Paragraph;
      P.In_Paragraph := True;
   end Parse_Preformatted;

   --  ------------------------------
   --  Parse a wiki heading.  The heading could start with '=' or '!'.
   --  The trailing equals are ignored.
   --  Example:
   --    == Level 2 ==
   --    !!! Level 3
   --  ------------------------------
   procedure Parse_Header (P     : in out Parser;
                           Token : in Wide_Wide_Character) is
      Header : Unbounded_Wide_Wide_String;
      C      : Wide_Wide_Character;
      Level  : Integer := 1;
   begin
      if not P.Empty_Line then
         Parse_Text (P, Token);
         return;
      end if;

      while Level <= 6 loop
         Peek (P, C);
         exit when C /= Token;
         Level := Level + 1;
      end loop;

      --  Ignore spaces after '=' signs
      while C = ' ' or C = HT loop
         Peek (P, C);
      end loop;

      loop
         Append (Header, C);
         Peek (P, C);
         exit when C = LF or C = CR;
      end loop;

      --  Remove the spaces and '=' at end of header string.
      declare
         Len          : Natural := Length (Header);
         Ignore_Token : Boolean := True;
         Seen_Token   : Boolean := False;
      begin
         while Len > 0 loop
            C := Element (Header, Len);
            if C = Token then
               exit when not Ignore_Token;
               Seen_Token := True;
            elsif C = ' ' or C = HT then
               Ignore_Token := not Seen_Token;
            else
               exit;
            end if;
            Delete (Header, Len, Len);
            Len := Len - 1;
         end loop;
      end;

      --  dotclear header is the opposite of Creole for the level.
      Level := Level + P.Header_Offset;
      if Level < 0 then
         Level := -Level;
      end if;
      if Level = 0 then
         Level := 1;
      end if;
      Flush_Text (P);
      P.Document.Add_Header (Header, Level);
      P.Empty_Line   := True;
      P.In_Paragraph := False;
   end Parse_Header;

   --  ------------------------------
   --  Parse a link.
   --  Example:
   --    [name]
   --    [name|url]
   --    [name|url|language]
   --    [name|url|language|title]
   --    [[link]]
   --    [[link|name]]
   --  ------------------------------
   procedure Parse_Link (P     : in out Parser;
                         Token : in Wide_Wide_Character) is

      --  Parse a link component
      procedure Parse_Link_Token (Into : in out Unbounded_Wide_Wide_String);

      Link       : Unbounded_Wide_Wide_String;
      Title      : Unbounded_Wide_Wide_String;
      Language   : Unbounded_Wide_Wide_String;
      Link_Title : Unbounded_Wide_Wide_String;
      C          : Wide_Wide_Character;

      procedure Parse_Link_Token (Into : in out Unbounded_Wide_Wide_String) is
      begin
         loop
            Peek (P, C);
            if C = P.Escape_Char then
               Peek (P, C);
            else
               exit when C = LF or C = CR or C = ']' or C = '|';
            end if;
            Append (Into, C);
         end loop;
      end Parse_Link_Token;

   begin
      --  If links have the form '[[link]]', check the second bracket.
      if P.Link_Double_Bracket then
         Peek (P, C);
         if C /= Token then
            Append (P.Text, Token);
            Put_Back (P, C);
            return;
         end if;
      end if;

      Parse_Link_Token (Title);
      if C = '|' then
         Parse_Link_Token (Link);
         if C = '|' then
            Parse_Link_Token (Language);
            if C = '|' then
               Parse_Link_Token (Link_Title);
            end if;
         end if;
      end if;
      if P.Link_Double_Bracket then
         Peek (P, C);
         if C /= ']' then
            Put_Back (P, C);
         end if;
      elsif C /= ']' then
         Put_Back (P, C);
      end if;
      P.Empty_Line := False;
      Flush_Text (P);
      P.Document.Add_Link (Title, Link, Language, Link_Title);
      Peek (P, C);
      if not P.Is_Eof then
         if C = CR or C = LF then
            Append (P.Text, C);
         end if;
         Put_Back (P, C);
      end if;
   end Parse_Link;

   --  ------------------------------
   --  Parse a quote.
   --  Example:
   --    {{name}}
   --    {{name|language}}
   --    {{name|language|url}}
   --  ------------------------------
   procedure Parse_Quote (P     : in out Parser;
                          Token : in Wide_Wide_Character) is

      --  Parse a quote component
      procedure Parse_Quote_Token (Into : in out Unbounded_Wide_Wide_String);

      Link       : Unbounded_Wide_Wide_String;
      Quote      : Unbounded_Wide_Wide_String;
      Language   : Unbounded_Wide_Wide_String;
      C          : Wide_Wide_Character;

      procedure Parse_Quote_Token (Into : in out Unbounded_Wide_Wide_String) is
      begin
         loop
            Peek (P, C);
            if C = P.Escape_Char then
               Peek (P, C);
            else
               exit when C = LF or C = CR or C = '}' or C = '|';
            end if;
            Append (Into, C);
         end loop;
      end Parse_Quote_Token;

   begin
      Peek (P, C);
      if C /= Token then
         Append (P.Text, Token);
         Put_Back (P, C);
         return;
      end if;

      Parse_Quote_Token (Quote);
      if C = '|' then
         Parse_Quote_Token (Language);
         if C = '|' then
            Parse_Quote_Token (Link);
         end if;
      end if;
      if C /= '}' then
         Put_Back (P, C);
      end if;
      Flush_Text (P);
      P.Document.Add_Quote (Quote, Link, Language);
      Peek (P, C);
      if C /= '}' then
         Put_Back (P, C);
      end if;
   end Parse_Quote;

   --  ------------------------------
   --  Parse an image.
   --  Example:
   --    ((url|alt text))
   --    ((url|alt text|position))
   --    ((url|alt text|position||description))
   --  ------------------------------
   procedure Parse_Image (P     : in out Parser;
                          Token : in Wide_Wide_Character) is

      --  Parse a image component
      procedure Parse_Image_Token (Into : in out Unbounded_Wide_Wide_String);

      Link       : Unbounded_Wide_Wide_String;
      Alt        : Unbounded_Wide_Wide_String;
      Position   : Unbounded_Wide_Wide_String;
      Desc       : Unbounded_Wide_Wide_String;
      C          : Wide_Wide_Character;

      procedure Parse_Image_Token (Into : in out Unbounded_Wide_Wide_String) is
      begin
         loop
            Peek (P, C);
            if C = P.Escape_Char then
               Peek (P, C);
            else
               exit when C = LF or C = CR or C = ')' or C = '|';
            end if;
            Append (Into, C);
         end loop;
      end Parse_Image_Token;

   begin
      Peek (P, C);
      if C /= Token then
         Append (P.Text, Token);
         Put_Back (P, C);
         return;
      end if;

      Parse_Image_Token (Link);
      if C = '|' then
         Parse_Image_Token (Alt);
         if C = '|' then
            Parse_Image_Token (Position);
            if C = '|' then
               Parse_Image_Token (Desc);
            end if;
         end if;
      end if;
      if C /= ')' then
         Put_Back (P, C);
      end if;
      Flush_Text (P);
      P.Document.Add_Image (Link, Alt, Position, Desc);
      Peek (P, C);
      if C /= ')' then
         Put_Back (P, C);
      end if;
   end Parse_Image;

   procedure Toggle_Format (P      : in out Parser;
                            Format : in Format_Type) is
   begin
      Flush_Text (P);
      P.Format (Format) := not P.Format (Format);
   end Toggle_Format;

   --  ------------------------------
   --  Parse the beginning or the end of a single character sequence.  This procedure
   --  is instantiated for several format types (bold, italic, superscript, subscript, code).
   --  Example:
   --    _name_    *bold*   `code`
   --  ------------------------------
   procedure Parse_Single_Format (P     : in out Parser;
                                  Token : in Wide_Wide_Character) is
      pragma Unreferenced (Token);
   begin
      Toggle_Format (P, Format);
   end Parse_Single_Format;

   procedure Parse_Single_Italic is new Parse_Single_Format (ITALIC);
   procedure Parse_Single_Bold is new Parse_Single_Format (BOLD);
   procedure Parse_Single_Code is new Parse_Single_Format (CODE);
   procedure Parse_Single_Superscript is new Parse_Single_Format (SUPERSCRIPT);
   --  procedure Parse_Single_Subscript is new Parse_Single_Format (SUBSCRIPT);
   --  procedure Parse_Single_Strikeout is new Parse_Single_Format (STRIKEOUT);

   --  ------------------------------
   --  Parse the beginning or the end of a double character sequence.  This procedure
   --  is instantiated for several format types (bold, italic, superscript, subscript, code).
   --  Example:
   --    --name--  **bold** ~~strike~~
   --  ------------------------------
   procedure Parse_Double_Format (P     : in out Parser;
                                  Token : in Wide_Wide_Character) is
      C : Wide_Wide_Character;
   begin
      Peek (P, C);
      if C = Token then
         Toggle_Format (P, Format);
      else
         Parse_Text (P, Token);
         Put_Back (P, C);
      end if;
   end Parse_Double_Format;

   procedure Parse_Double_Italic is new Parse_Double_Format (ITALIC);
   procedure Parse_Double_Bold is new Parse_Double_Format (BOLD);
   procedure Parse_Double_Code is new Parse_Double_Format (CODE);
   --  procedure Parse_Double_Superscript is new Parse_Double_Format (SUPERSCRIPT);
   procedure Parse_Double_Subscript is new Parse_Double_Format (SUBSCRIPT);
   procedure Parse_Double_Strikeout is new Parse_Double_Format (STRIKEOUT);

   --  ------------------------------
   --  Parse an italic, bold or bold + italic sequence.
   --  Example:
   --    ''name''         (italic)
   --    '''name'''       (bold)
   --    '''''name'''''   (bold+italic)
   --  ------------------------------
   procedure Parse_Bold_Italic (P     : in out Parser;
                                Token : in Wide_Wide_Character) is
      C     : Wide_Wide_Character;
      Count : Natural := 1;
   begin
      loop
         Peek (P, C);
         exit when C /= Token;
         Count := Count + 1;
      end loop;
      if Count > 10 then
         Count := Count mod 10;
         if Count = 0 then
            Put_Back (P, C);
            return;
         end if;
      end if;

      case Count is
         when 1 =>
            Parse_Text (P, Token);

         when 2 =>
            Toggle_Format (P, ITALIC);

         when 3 =>
            Toggle_Format (P, BOLD);

         when 4 =>
            Toggle_Format (P, BOLD);
            Parse_Text (P, Token);

         when 5 =>
            Toggle_Format (P, BOLD);
            Toggle_Format (P, ITALIC);

         when others =>
            null;
      end case;
      Put_Back (P, C);
   end Parse_Bold_Italic;

   procedure Parse_List (P     : in out Parser;
                         Token : in Wide_Wide_Character) is
      C     : Wide_Wide_Character;
      Level : Natural := 1;
   begin
      if not P.Empty_Line then
         Parse_Text (P, Token);
         return;
      end if;
      loop
         Peek (P, C);
         exit when C /= '#' and C /= '*';
         Level := Level + 1;
      end loop;
      Flush_Text (P);
      P.Document.Add_List_Item (Level, Token = '#');

      --  Ignore the first white space after the list item.
      if C /= ' ' and C /= HT then
         Put_Back (P, C);
      end if;
   end Parse_List;

   --  ------------------------------
   --  Parse a blockquote.
   --  Example:
   --    >>>quote level 3
   --    >>quote level 2
   --    >quote level 1
   --  ------------------------------
   procedure Parse_Blockquote (P     : in out Parser;
                               Token : in Wide_Wide_Character) is
      C     : Wide_Wide_Character;
      Level : Natural := 1;
   begin
      if not P.Empty_Line then
         Parse_Text (P, Token);
         return;
      end if;
      loop
         Peek (P, C);
         exit when C /= '>';
         Level := Level + 1;
      end loop;
      Flush_Text (P);
      P.Empty_Line := True;
      P.Quote_Level := Level;
      P.Document.Add_Blockquote (Level);

      --  Ignore the first white space after the quote character.
      if C /= ' ' and C /= HT then
         Put_Back (P, C);
      end if;
   end Parse_Blockquote;

   --  ------------------------------
   --  Parse a space and take necessary formatting actions.
   --  Example:
   --    item1 item2   => add space in text buffer
   --    ' * item'     => start a bullet list (Google)
   --    ' # item'     => start an ordered list (Google)
   --    ' item'       => preformatted text (Google, Creole)
   --  ------------------------------
   procedure Parse_Space (P     : in out Parser;
                          Token : in Wide_Wide_Character) is
      C     : Wide_Wide_Character;
   begin
      if P.Empty_Line then
         loop
            Peek (P, C);
            exit when C /= ' ' and C /= HT;
         end loop;
         if C = '*' or C = '#' then
            Parse_List (P, C);
         elsif C = CR or C = LF then
            Parse_End_Line (P, C);
         else
            Put_Back (P, C);
            Parse_Preformatted (P, Token);
         end if;
      else
         Append (P.Text, Token);
      end if;
   end Parse_Space;

   procedure Parse_End_Line (P     : in out Parser;
                             Token : in Wide_Wide_Character) is
      C     : Wide_Wide_Character := Token;
      Count : Positive := 1;
   begin
      if P.Is_Eof then
         return;
      end if;
      while not P.Is_Eof loop
         Peek (P, C);
         exit when C /= CR and C /= LF;
         if C = Token then
            Count := Count + 1;
         end if;
      end loop;
      Put_Back (P, C);
      if Count >= 2 then
         Flush_Text (P);

         --  Finish the active blockquotes if a new paragraph is started on an empty line.
         if P.Quote_Level > 0 then
            P.Document.Add_Blockquote (0);
            P.Quote_Level := 0;
         end if;
         P.Document.Add_Paragraph;
         P.In_Paragraph := True;
      elsif Length (P.Text) > 0 or not P.Empty_Line then
         Append (P.Text, Token);
      end if;

      --  Finish the active blockquotes if a new paragraph is started immediately after
      --  the blockquote.
      if P.Quote_Level > 0 and C /= '>' then
         Flush_Text (P);
         P.Document.Add_Blockquote (0);
         P.Quote_Level := 0;
      end if;
      P.Empty_Line := True;
   end Parse_End_Line;

   --  ------------------------------
   --  Parse a line break.
   --  Example:
   --     \\    (Creole)
   --     %%%   (Dotclear)
   --  ------------------------------
   procedure Parse_Line_Break (P     : in out Parser;
                               Token : in Wide_Wide_Character) is
      C : Wide_Wide_Character;
   begin
      Peek (P, C);

      --  Check for escape character
      if Token = P.Escape_Char then
         Parse_Text (P, C);
         return;
      end if;
      if C /= Token then
         Parse_Text (P, Token);
         Put_Back (P, C);
         return;
      end if;

      --  Check for a third '%'.
      if P.Is_Dotclear then
         Peek (P, C);
         if C /= Token then
            Parse_Text (P, Token);
            Parse_Text (P, Token);
            Put_Back (P, C);
            return;
         end if;
      end if;
      P.Empty_Line := True;
      Flush_Text (P);
      P.Document.Add_Line_Break;
   end Parse_Line_Break;

   Google_Wiki_Table : constant Parser_Table
     := (
       16#0A# => Parse_End_Line'Access,
       16#0D# => Parse_End_Line'Access,
       Character'Pos (' ') => Parse_Space'Access,
       Character'Pos ('=') => Parse_Header'Access,
       Character'Pos ('*') => Parse_Single_Bold'Access,
       Character'Pos ('_') => Parse_Single_Italic'Access,
       Character'Pos ('`') => Parse_Single_Code'Access,
       Character'Pos ('^') => Parse_Single_Superscript'Access,
       Character'Pos ('~') => Parse_Double_Strikeout'Access,
       Character'Pos (',') => Parse_Double_Subscript'Access,
       Character'Pos ('[') => Parse_Link'Access,
       Character'Pos ('\') => Parse_Line_Break'Access,
       Character'Pos ('#') => Parse_List'Access,
       Character'Pos ('{') => Parse_Preformatted'Access,
       Character'Pos ('}') => Parse_Preformatted'Access,
       others => Parse_Text'Access
      );

   Dotclear_Wiki_Table : constant Parser_Table
     := (
       16#0A# => Parse_End_Line'Access,
       16#0D# => Parse_End_Line'Access,
       Character'Pos (' ') => Parse_Space'Access,
       Character'Pos ('!') => Parse_Header'Access,
       Character'Pos ('_') => Parse_Double_Bold'Access,
       Character'Pos (''') => Parse_Double_Italic'Access,
       Character'Pos ('@') => Parse_Double_Code'Access,
       Character'Pos ('^') => Parse_Single_Superscript'Access,
       Character'Pos ('-') => Parse_Double_Strikeout'Access,
       Character'Pos ('+') => Parse_Double_Strikeout'Access,
       Character'Pos (',') => Parse_Double_Subscript'Access,
       Character'Pos ('[') => Parse_Link'Access,
       Character'Pos ('\') => Parse_Line_Break'Access,
       Character'Pos ('{') => Parse_Quote'Access,
       Character'Pos ('#') => Parse_List'Access,
       Character'Pos ('*') => Parse_List'Access,
       Character'Pos ('(') => Parse_Image'Access,
       Character'Pos ('/') => Parse_Preformatted'Access,
       Character'Pos ('%') => Parse_Line_Break'Access,
       Character'Pos ('>') => Parse_Blockquote'Access,
       others => Parse_Text'Access
      );

   Creole_Wiki_Table : constant Parser_Table
     := (
       16#0A# => Parse_End_Line'Access,
       16#0D# => Parse_End_Line'Access,
       Character'Pos (' ') => Parse_Space'Access,
       Character'Pos ('=') => Parse_Header'Access,
       Character'Pos ('*') => Parse_Double_Bold'Access,
       Character'Pos ('/') => Parse_Double_Italic'Access,
       Character'Pos ('@') => Parse_Double_Code'Access,
       Character'Pos ('^') => Parse_Single_Superscript'Access,
       Character'Pos ('-') => Parse_Double_Strikeout'Access,
       Character'Pos ('+') => Parse_Double_Strikeout'Access,
       Character'Pos (',') => Parse_Double_Subscript'Access,
       Character'Pos ('[') => Parse_Link'Access,
       Character'Pos ('\') => Parse_Line_Break'Access,
       Character'Pos ('#') => Parse_List'Access,
       Character'Pos ('{') => Parse_Image'Access,
       Character'Pos ('%') => Parse_Line_Break'Access,
       others => Parse_Text'Access
      );

   Mediawiki_Wiki_Table : constant Parser_Table
     := (
       16#0A# => Parse_End_Line'Access,
       16#0D# => Parse_End_Line'Access,
       Character'Pos (' ') => Parse_Space'Access,
       Character'Pos ('=') => Parse_Header'Access,
       Character'Pos (''') => Parse_Bold_Italic'Access,
       Character'Pos ('[') => Parse_Link'Access,
       Character'Pos ('\') => Parse_Line_Break'Access,
       Character'Pos ('{') => Parse_Quote'Access,
       Character'Pos ('#') => Parse_List'Access,
       Character'Pos ('*') => Parse_List'Access,
       others => Parse_Text'Access
      );

   Misc_Wiki_Table : constant Parser_Table
     := (
       16#0A# => Parse_End_Line'Access,
       16#0D# => Parse_End_Line'Access,
       Character'Pos (' ') => Parse_Space'Access,
       Character'Pos ('=') => Parse_Header'Access,
       Character'Pos ('*') => Parse_Single_Bold'Access,
       Character'Pos ('_') => Parse_Single_Italic'Access,
       Character'Pos ('`') => Parse_Single_Code'Access,
       Character'Pos ('^') => Parse_Single_Superscript'Access,
       Character'Pos ('~') => Parse_Double_Strikeout'Access,
       Character'Pos (',') => Parse_Double_Subscript'Access,
       Character'Pos ('[') => Parse_Link'Access,
       Character'Pos ('\') => Parse_Line_Break'Access,
       Character'Pos ('#') => Parse_List'Access,
       Character'Pos ('@') => Parse_Double_Code'Access,
       others => Parse_Text'Access
      );

   --  ------------------------------
   --  Parse the wiki text contained in <b>Text</b> according to the wiki syntax
   --  specified in <b>Syntax</b> and invoke the document reader procedures defined
   --  by <b>into</b>.
   --  ------------------------------
   procedure Parse (Into   : in AWA.Wikis.Documents.Document_Reader_Access;
                    Text   : in Wide_Wide_String;
                    Syntax : in Wiki_Syntax_Type := SYNTAX_MIX) is

      type Wide_Input is new Input with record
         Pos : Positive;
      end record;

      procedure Read_Char (Buf    : in out Wide_Input;
                           Token  : out Wide_Wide_Character;
                           Is_Eof : out Boolean);

      procedure Read_Char (Buf    : in out Wide_Input;
                           Token  : out Wide_Wide_Character;
                           Is_Eof : out Boolean) is
      begin
         if Buf.Pos > Text'Last then
            Is_Eof := True;
            Token := CR;
         else
            Token := Text (Buf.Pos);
            Buf.Pos := Buf.Pos + 1;
            Is_Eof := False;
         end if;
      end Read_Char;

      P      : Parser;
      Buffer : aliased Wide_Input;
   begin
      Buffer.Pos   := Text'First;
      P.Document   := Into;
      P.Empty_Line := True;
      P.Format     := (others => False);
      P.Is_Eof     := False;
      P.Has_Pending := False;
      P.Reader      := Buffer'Unchecked_Access;
      P.Link_Double_Bracket := False;
      P.Escape_Char := '~';
      case Syntax is
         when SYNTAX_GOOGLE =>
            Parse_Token (P, Google_Wiki_Table);

         when SYNTAX_DOTCLEAR =>
            P.Is_Dotclear := True;
            P.Escape_Char := '\';
            P.Header_Offset := -6;
            Parse_Token (P, Dotclear_Wiki_Table);

         when SYNTAX_CREOLE =>
            P.Link_Double_Bracket := True;
            Parse_Token (P, Creole_Wiki_Table);

         when SYNTAX_MEDIA_WIKI | SYNTAX_PHPBB =>
            Parse_Token (P, Mediawiki_Wiki_Table);

         when SYNTAX_MIX =>
            P.Is_Dotclear := True;
            Parse_Token (P, Misc_Wiki_Table);

      end case;
   end Parse;

   procedure Parse_Token (P     : in out Parser;
                          Table : in Parser_Table) is
      C : Wide_Wide_Character;
   begin
      P.Document.Add_Paragraph;
      P.In_Paragraph := True;
      while not P.Is_Eof loop
         Peek (P, C);
         if C > '~' then
            Parse_Text (P, C);
         else
            Table (Wide_Wide_Character'Pos (C)).all (P, C);
         end if;
      end loop;

      Flush_Text (P);
      P.Document.Finish;
   end Parse_Token;

end AWA.Wikis.Parsers;
