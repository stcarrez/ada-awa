-----------------------------------------------------------------------
--  awa-tests-helpers - Helpers for AWA unit tests
--  Copyright (C) 2011, 2017, 2020 Stephane Carrez
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
with ADO;
with ASF.Responses.Mockup;
with Ada.Strings.Unbounded;
package AWA.Tests.Helpers is

   --  Extract from the Location header the part that is after the given base string.
   --  If the Location header does not start with the base string, returns the empty
   --  string.
   function Extract_Redirect (Reply : in ASF.Responses.Mockup.Response'Class;
                              Base  : in String) return String;

   function Extract_Redirect (Reply : in ASF.Responses.Mockup.Response'Class;
                              Base  : in String) return Ada.Strings.Unbounded.Unbounded_String;

   --  Extract from the response content a link with a given title.
   function Extract_Link (Content : in String;
                          Title   : in String) return String;

   --  Extract from the response content an HTML identifier that was generated
   --  with the given prefix.   The format is assumed to be <prefix>-<number>.
   function Extract_Identifier (Content : in String;
                                Prefix  : in String) return ADO.Identifier;

end AWA.Tests.Helpers;
