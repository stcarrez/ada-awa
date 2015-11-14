-----------------------------------------------------------------------
--  awa-counters-definition -- Counter definition
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
