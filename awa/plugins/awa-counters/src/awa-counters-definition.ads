-----------------------------------------------------------------------
--  awa-counters-definition -- Counter definition
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <tt>AWA.Counters.Definition</tt> package is instantiated for each counter definition.
generic
   Table : ADO.Schemas.Class_Mapping_Access;
   Field : String := "";
package AWA.Counters.Definition is

   Def_Name : aliased constant String := Field;

   --  Get the counter definition index.
   function Index return Counter_Index_Type;

private

   package Def is new Counter_Arrays.Definition (Counter_Def '(Table, Def_Name'Access));

   --  Get the counter definition index.
   function Index return Counter_Index_Type renames Def.Kind;

end AWA.Counters.Definition;
