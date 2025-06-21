-----------------------------------------------------------------------
--  awa-tests-helpers - Helpers for AWA unit tests
--  Copyright (C) 2011, 2017, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
