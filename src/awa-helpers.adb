-----------------------------------------------------------------------
--  awa-helpers -- Helpers for AWA applications
--  Copyright (C) 2011, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body AWA.Helpers is

   --  ------------------------------
   --  Get the value as an identifier.
   --  Returns NO_IDENTIFIER if the value is invalid.
   --  ------------------------------
   function To_Identifier (Value : in Util.Beans.Objects.Object) return ADO.Identifier is
   begin
      return ADO.Identifier (Util.Beans.Objects.To_Long_Long_Integer (Value));

   exception
      when Constraint_Error =>
         return ADO.NO_IDENTIFIER;
   end To_Identifier;

end AWA.Helpers;
