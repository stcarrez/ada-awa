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
with ADO.Objects;
with ADO.Schemas;
with Util.Strings;
with AWA.Index_Arrays;

--  == Introduction ==
--  The <b>Counters</b> module defines a general purpose counter service that allows to
--  associate counters to database entities.  For example it can be used to track the number
--  of times a blog post or a wiki page is accessed.  The <b>Counters</b> module maintains the
--  counters in a table on a per-day and per-entity basis.  It allows to update the full counter
--  in the target database entity table.
--
--  @include awa-counters-modules.ads
--
--  === Counter Declaration ===
--  Each counter must be declared by instantiating the <b>Definition</b> package.
--  This instantiation serves as identification of the counter and it defines the database
--  table as well as the column in that table that will hold the total counter.  The following
--  definition is used for the read counter of a wiki page.  The wiki page table contains a
--  <i>read_count</i> column and it will be incremented each time the counter is incremented.
--
--     package Read_Counter is
--        new AWA.Counters.Definition (AWA.Wikis.Models.WIKI_PAGE_TABLE, "read_count");
--
--  When the database table does not contain any counter column, the counter definition is
--  defined as follows:
--
--     package Login_Counter is
--        new AWA.Counters.Definition (AWA.Users.Models.USER_PAGE_TABLE);
--
--  Sometimes a counter is not associated with any database entity.  Such counters are global
--  and they are assigned a unique name.
--
--     package Start_Counter is
--        new AWA.Counters.Definition (null, "startup_counter");
--
--  === Incrementing the counter ===
--  Incrementing the counter is done by calling the <b>Increment</b> operation.
--  When the counter is associated with a database entity, the entity primary key must be given.
--
--     AWA.Counters.Increment (Counter => Read_Counter.Counter, Key => Id);
--
--  A global counter is also incremented by using the <b>Increment</b> operation.
--
--     AWA.Counters.Increment (Counter => Start_Counter.Counter);
--
--  @include awa-counters-beans.ads
--  @include awa-counters-components.ads
--  @include counters.xml
--
--  == Model ==
--  [images/awa_counters_model.png]
--
package AWA.Counters is

   type Counter_Index_Type is new Natural;

   --  Increment the counter identified by <tt>Counter</tt> and associated with the
   --  database object <tt>Object</tt>.
   procedure Increment (Counter : in Counter_Index_Type;
                        Object  : in ADO.Objects.Object_Ref'Class);

   --  Increment the counter identified by <tt>Counter</tt> and associated with the
   --  database object key <tt>Key</tt>.
   procedure Increment (Counter : in Counter_Index_Type;
                        Key     : in ADO.Objects.Object_Key);

   --  Increment the global counter identified by <tt>Counter</tt>.
   procedure Increment (Counter : in Counter_Index_Type);

private

   type Counter_Def is record
      Table : ADO.Schemas.Class_Mapping_Access;
      Field : Util.Strings.Name_Access;
   end record;

   function "=" (Left, Right : in Counter_Def) return Boolean;
   function "<" (Left, Right : in Counter_Def) return Boolean;
   function "&" (Left  : in String;
                 Right : in Counter_Def) return String;

   package Counter_Arrays is new AWA.Index_Arrays (Counter_Index_Type, Counter_Def);

end AWA.Counters;
