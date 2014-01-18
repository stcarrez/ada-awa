-----------------------------------------------------------------------
--  awa-helpers-requests -- Helpers for AWA applications
--  Copyright (C) 2011, 2012, 2013, 2014 Stephane Carrez
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

with Ada.Calendar;
with ADO;
with Util.Beans.Objects;
package AWA.Helpers.Requests is

   --  Get the parameter identified by the given name and return it as an identifier.
   --  Returns NO_IDENTIFIER if the parameter does not exist or is not valid.
   function Get_Parameter (Name : in String) return ADO.Identifier;

   --  Get the parameter identified by the given name and return it as an integer.
   --  Returns the default value if the parameter does not exist or is not valid.
   function Get_Parameter (Name    : in String;
                           Default : in Integer) return Integer;

   --  Get the parameter identified by the given name and return it as a string.
   --  Returns the default value if the parameter does not exist or is not valid.
   function Get_Parameter (Name    : in String;
                           Default : in String) return String;

   --  Get the parameter identified by the given name and return it as a date.
   --  Returns the default value if the parameter does not exist or is not a valid date.
   --  The date is assumed to be in ISO8601 format.
   function Get_Parameter (Name    : in String;
                           Default : in String) return Ada.Calendar.Time;

   --  Get the parameter identified by the given name and return it as an Object.
   --  Returns the NULL object value if the parameter does not exist.
   function Get_Parameter (Name    : in String) return Util.Beans.Objects.Object;

end AWA.Helpers.Requests;
