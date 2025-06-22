-----------------------------------------------------------------------
--  awa-index_arrays -- Static index arrays
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

generic
   type Index_Type is range <>;
   type Element_Type (<>) is private;
   with function "=" (Left, Right : in Element_Type) return Boolean is <>;
   with function "<" (Left, Right : in Element_Type) return Boolean is <>;
   with function "&" (Left  : in String;
                      Right : in Element_Type) return String is <>;
package AWA.Index_Arrays is

   --  This package must be instantiated for each definition.
   --  It allocates a unique <tt>Index_Type</tt> value for each definition.
   generic
      Name : Element_Type;
   package Definition is
      function Kind return Index_Type;
      pragma Inline_Always (Kind);
   end Definition;

   type Element_Type_Access is access constant Element_Type;

   --  Exception raised if a name is not found.
   Not_Found     : exception;

   --  Identifies an invalid index.
   Invalid_Index : constant Index_Type;

   --  Find the runtime index given the name.
   --  Raises Not_Found exception if the name is not recognized.
   function Find (Name : in Element_Type) return Index_Type;

   --  Get the element associated with the index.
   function Get_Element (Index : in Index_Type) return Element_Type_Access;

   --  Check if the index is a valid index.
   function Is_Valid (Index : in Index_Type) return Boolean;

   --  Get the last valid index.
   function Get_Last return Index_Type;

private

   Invalid_Index : constant Index_Type := Index_Type'First;

   pragma Inline (Is_Valid);
   pragma Inline (Get_Last);

end AWA.Index_Arrays;
