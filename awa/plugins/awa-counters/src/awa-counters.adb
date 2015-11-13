-----------------------------------------------------------------------
--  awa-counters --
--  Copyright (C) 2015 Stephane Carrez
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

with AWA.Counters.Modules;
package body AWA.Counters is

   use type Util.Strings.Name_Access;
   use type ADO.Schemas.Class_Mapping_Access;

   function "&" (Left  : in String;
                 Right : in Counter_Def) return String is
   begin
      return Left & "[" & Right.Table.Table.all & ", " & Right.Field.all & "]";
   end "&";

   function "=" (Left, Right : in Counter_Def) return Boolean is
   begin
      return Left.Table = Right.Table and Left.Field = Right.Field;
   end "=";

   function "<" (Left, Right : in Counter_Def) return Boolean is
   begin
      return Left.Table.Table.all < Right.Table.Table.all
        or (Left.Table = Right.Table and Left.Field.all < Right.Field.all);
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

end AWA.Counters;
