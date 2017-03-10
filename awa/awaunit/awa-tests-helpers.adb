-----------------------------------------------------------------------
--  awa-tests-helpers - Helpers for AWA unit tests
--  Copyright (C) 2011, 2017 Stephane Carrez
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

end AWA.Tests.Helpers;
