-----------------------------------------------------------------------
--  awa-counters-modules -- Module counters
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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Calendar;

with ASF.Applications;

with AWA.Modules;
package AWA.Counters.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "counters";

   --  Default age limit to flush counters: 5 minutes.
   DEFAULT_AGE_LIMIT : constant Duration := 5 * 60.0;

   --  Default maximum number of different counters to keep before flushing.
   DEFAULT_COUNTER_LIMIT : constant Natural := 1_000;

   PARAM_AGE_LIMIT       : constant String := "counter_age_limit";

   PARAM_COUNTER_LIMIT   : constant String := "counter_limit";

   --  ------------------------------
   --  Module counters
   --  ------------------------------
   type Counter_Module is new AWA.Modules.Module with private;
   type Counter_Module_Access is access all Counter_Module'Class;

   --  Initialize the counters module.
   overriding
   procedure Initialize (Plugin : in out Counter_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Configures the module after its initialization and after having read its XML configuration.
   overriding
   procedure Configure (Plugin : in out Counter_Module;
                        Props  : in ASF.Applications.Config);

   --  Get the counters module.
   function Get_Counter_Module return Counter_Module_Access;

   --  Increment the counter identified by <tt>Counter</tt> and associated with the
   --  database object <tt>Object</tt>.
   procedure Increment (Plugin  : in out Counter_Module;
                        Counter : in Counter_Index_Type;
                        Object  : in ADO.Objects.Object_Ref'Class);

   --  Get the current counter value.
   procedure Get_Counter (Plugin  : in out Counter_Module;
                          Counter : in AWA.Counters.Counter_Index_Type;
                          Object  : in ADO.Objects.Object_Ref'Class;
                          Result  : out Natural);

   --  Flush the existing counters and update all the database records refered to them.
   procedure Flush (Plugin : in out Counter_Module);

private

   type Definition_Array_Type is array (Counter_Index_Type range <>) of Natural;
   type Definition_Array_Type_Access is access all Definition_Array_Type;

   --  The counter map tracks a counter associated with a database object.
   --  All the database objects refer to the same counter.
   package Counter_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => ADO.Objects.Object_Key,
                                                Element_Type    => Positive,
                                                Hash            => ADO.Objects.Hash,
                                                Equivalent_Keys => ADO.Objects."=");

   --  The <tt>Counter_Map_Array</tt> associate a counter map to each counter definition.
   type Counter_Map_Array is array (Counter_Index_Type range <>) of Counter_Maps.Map;
   type Counter_Map_Array_Access is access all Counter_Map_Array;

   --  Counters are kept temporarily in the <tt>Counter_Table</tt> protected type to avoid
   --  having to update the database each time a counter is incremented.  Counters are flushed
   --  when the table reaches some limit, or, when the table is oldest than some limit.
   --  Counters are associated with a day so that it becomes possible to gather per-day counters.
   --
   --  The <tt>Flush</tt> operation on the <tt>Counter_Module</tt> can be used to flush
   --  the pending counters.  For each counter that was updated, it either inserts or
   --  updates a row in the counters database table.  Because such operation is slow, it is not
   --  implemented in the protected type.  Instead, we steal the counter table and replace it
   --  with a new/empty table.  This is done by <tt>Steal_Counters</tt> protected operation.
   --  While doing the flush, other tasks can increment counters without being blocked by the
   --  <tt>Flush</tt> operation.
   protected type Counter_Table is

      --  Increment the counter identified by <tt>Counter</tt> and associated with the
      --  database object <tt>Object</tt>.
      procedure Increment (Counter : in Counter_Index_Type;
                           Object  : in ADO.Objects.Object_Ref'Class);

      --  Get the counters that have been collected with the date and prepare to collect
      --  new counters.
      procedure Steal_Counters (Result : out Counter_Map_Array_Access;
                                Date   : out Ada.Calendar.Time);

      --  Check if we must flush the counters.
      function Need_Flush (Limit   : in Natural;
                           Seconds : in Duration) return Boolean;

      --  Get the definition ID associated with the counter.
      procedure Get_Definition (Counter : in Counter_Index_Type;
                                Result  : out Natural);

   private
      Day         : Ada.Calendar.Time;
      Day_End     : Ada.Calendar.Time;
      Counters    : Counter_Map_Array_Access;
      Definitions : Definition_Array_Type_Access;
      Nb_Counters : Natural := 0;
   end Counter_Table;

   type Counter_Module is new AWA.Modules.Module with record
      Counters      : Counter_Table;

      Counter_Limit : Natural  := DEFAULT_COUNTER_LIMIT;
      Age_Limit     : Duration := DEFAULT_AGE_LIMIT;
   end record;

end AWA.Counters.Modules;
