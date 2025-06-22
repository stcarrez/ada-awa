-----------------------------------------------------------------------
--  awa-helpers -- Helpers for AWA applications
--  Copyright (C) 2011, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO;
with Util.Beans.Objects;
package AWA.Helpers is

   --  Get the value as an identifier.
   --  Returns NO_IDENTIFIER if the value is invalid.
   function To_Identifier (Value : in Util.Beans.Objects.Object) return ADO.Identifier;

end AWA.Helpers;
