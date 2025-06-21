-----------------------------------------------------------------------
--  awa-counters --
--  Copyright (C) 2015, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Counters.Modules;
package body AWA.Counters is

   use type Util.Strings.Name_Access;
   use type ADO.Schemas.Class_Mapping_Access;

   function "&" (Left  : in String;
                 Right : in Counter_Def) return String is
   begin
      if Right.Table = null then
         return Left & "[" & Right.Field.all & "]";
      else
         return Left & "[" & Right.Table.Table.all & ", " & Right.Field.all & "]";
      end if;
   end "&";

   overriding
   function "=" (Left, Right : in Counter_Def) return Boolean is
   begin
      return Left.Table = Right.Table and then Left.Field = Right.Field;
   end "=";

   function "<" (Left, Right : in Counter_Def) return Boolean is
   begin
      if Left.Table = Right.Table then
         return Left.Field.all < Right.Field.all;
      elsif Left.Table = null then
         return False;
      elsif Right.Table = null then
         return True;
      else
         return Left.Table.Table.all < Right.Table.Table.all;
      end if;
   end "<";

   --  ------------------------------
   --  Increment the counter identified by <tt>Counter</tt> and associated with the
   --  database object <tt>Object</tt>.
   --  ------------------------------
   procedure Increment (Counter : in Counter_Index_Type;
                        Object  : in ADO.Objects.Object_Ref'Class) is
      Module : constant AWA.Counters.Modules.Counter_Module_Access
        := AWA.Counters.Modules.Get_Counter_Module;
   begin
      Module.Increment (Counter, Object);
   end Increment;

   --  ------------------------------
   --  Increment the counter identified by <tt>Counter</tt> and associated with the
   --  database object key <tt>Key</tt>.
   --  ------------------------------
   procedure Increment (Counter : in Counter_Index_Type;
                        Key     : in ADO.Objects.Object_Key) is
      Module : constant AWA.Counters.Modules.Counter_Module_Access
        := AWA.Counters.Modules.Get_Counter_Module;
   begin
      Module.Increment (Counter, Key);
   end Increment;

   --  ------------------------------
   --  Increment the global counter identified by <tt>Counter</tt>.
   --  ------------------------------
   procedure Increment (Counter : in Counter_Index_Type) is
      Module : constant AWA.Counters.Modules.Counter_Module_Access
        := AWA.Counters.Modules.Get_Counter_Module;
   begin
      Module.Increment (Counter);
   end Increment;

end AWA.Counters;
