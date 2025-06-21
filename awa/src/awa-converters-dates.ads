-----------------------------------------------------------------------
--  awa-converters-dates -- Date Converters
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Converters;
with ASF.Converters.Dates;
with ASF.Contexts.Faces;
with ASF.Components.Base;
with Util.Beans.Objects;

package AWA.Converters.Dates is

   --  ------------------------------
   --  Relative Date Converter
   --  ------------------------------
   --  The <b>Relative_Date_Converter</b> translates a date value into a relative presentation
   --  for a human.  The date is converted relatively to the current time to display a message
   --  in one of the following forms:
   --
   --    o 'a moment ago', if D - now < 60 secs
   --    o 'X minutes ago', if D - now < 60 mins
   --    o 'X hours ago', if D - now < 24 hours
   --    o 'X days ago', if D - now < 7 days
   --    o date

   type Relative_Date_Converter is new ASF.Converters.Dates.Date_Converter with null record;
   type Relative_Date_Converter_Access is access all Relative_Date_Converter'Class;

   --  Convert the object value into a string.  The object value is associated
   --  with the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   overriding
   function To_String (Convert   : in Relative_Date_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in Util.Beans.Objects.Object) return String;

end AWA.Converters.Dates;
