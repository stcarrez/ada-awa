-----------------------------------------------------------------------
--  awa-tests-helpers - Helpers for AWA unit tests
--  Copyright (C) 2011, 2017, 2018, 2020 Stephane Carrez
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
with GNAT.Regpat;
package body AWA.Tests.Helpers is

   --  ------------------------------
   --  Extract from the Location header the part that is after the given base string.
   --  If the Location header does not start with the base string, returns the empty
   --  string.
   --  ------------------------------
   function Extract_Redirect (Reply : in ASF.Responses.Mockup.Response'Class;
                              Base  : in String) return String is
      R : constant String := Reply.Get_Header ("Location");
   begin
      if R'Length < Base'Length then
         return "";
      elsif R (R'First .. R'First + Base'Length - 1) /= Base then
         return "";
      else
         return R (R'First + Base'Length .. R'Last);
      end if;
   end Extract_Redirect;

   function Extract_Redirect (Reply : in ASF.Responses.Mockup.Response'Class;
                              Base  : in String) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String (Extract_Redirect (Reply, Base));
   end Extract_Redirect;

   --  ------------------------------
   --  Extract from the response content a link with a given title.
   --  ------------------------------
   function Extract_Link (Content : in String;
                          Title   : in String) return String is
      use GNAT.Regpat;

      Pattern : constant String := " href=""([a-zA-Z0-9/]+)"">" & Title & "</a>";
      Regexp  : constant Pattern_Matcher := Compile (Expression => Pattern);
      Result  : GNAT.Regpat.Match_Array (0 .. 1);
   begin
      Match (Regexp, Content, Result);
      if Result (1) = GNAT.Regpat.No_Match then
         return "";
      end if;
      return Content (Result (1).First .. Result (1).Last);
   end Extract_Link;

   --  ------------------------------
   --  Extract from the response content an HTML identifier that was generated
   --  with the given prefix.   The format is assumed to be <prefix>-<number>.
   --  ------------------------------
   function Extract_Identifier (Content : in String;
                                Prefix  : in String) return ADO.Identifier is
      use GNAT.Regpat;

      Pattern : constant String := ".*[\\""']" & Prefix & "\-([0-9]+)[\\""']";
      Regexp  : constant Pattern_Matcher := Compile (Expression => Pattern);
      Result  : GNAT.Regpat.Match_Array (0 .. 1);
   begin
      Match (Regexp, Content, Result);
      if Result (1) = GNAT.Regpat.No_Match then
         return ADO.NO_IDENTIFIER;
      end if;
      return ADO.Identifier'Value (Content (Result (1).First .. Result (1).Last));

   exception
      when Constraint_Error =>
         return ADO.NO_IDENTIFIER;
   end Extract_Identifier;

end AWA.Tests.Helpers;
