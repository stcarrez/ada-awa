-----------------------------------------------------------------------
--  awa-counters-modules -- Module counters
--  Copyright (C) 2015, 2018, 2020 Stephane Carrez
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
with ADO.Sessions;
with AWA.Modules;

--  == Integration ==
--  The `Counter_Module` manages the counters associated with database entities.
--  To avoid having to update the database each time a counter is incremented,
--  counters are kept temporarily in a `Counter_Table` protected type.
--  The table contains only the partial increments and not the real counter
--  values.  Counters are flushed when the table reaches some limit, or,
--  when the table is oldest than some limit.  Counters are associated with
--  a day so that it becomes possible to gather per-day counters.
--  The table is also flushed when a counter is incremented in a different day.
--
--  To be able to use the `Counters` module, you will need to add the
--  following line in your GNAT project file:
--
--    with "awa_counters";
--  
--  An instance of the `Counter_Module` must be declared and registered in the
--  AWA application.  The module instance can be defined as follows:
--
--    with AWA.Counters.Modules;
--    ...
--    type Application is new AWA.Applications.Application with record
--       Counter_Module : aliased AWA.Counters.Modules.Counter_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Counters.Modules.NAME,
--              Module => App.Counter_Module'Access);
--
--  == Configuration ==
--  The `counters` module defines the following configuration parameters:
--
--  @include-config counters.xml
--  
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

   --  Configures the module after its initialization and after having
   --  read its XML configuration.
   overriding
   procedure Configure (Plugin : in out Counter_Module;
                        Props  : in ASF.Applications.Config);

   --  Get the counters module.
   function Get_Counter_Module return Counter_Module_Access;

   --  Increment the counter identified by `Counter` and associated with the
   --  database object `Object`.
   procedure Increment (Plugin  : in out Counter_Module;
                        Counter : in Counter_Index_Type;
                        Object  : in ADO.Objects.Object_Ref'Class);

   --  Increment the counter identified by `Counter` and associated with the
   --  database object key `Key`.
   procedure Increment (Plugin  : in out Counter_Module;
                        Counter : in Counter_Index_Type;
                        Key     : in ADO.Objects.Object_Key);

   --  Increment the counter identified by `Counter`.
   procedure Increment (Plugin  : in out Counter_Module;
                        Counter : in Counter_Index_Type);

   --  Get the current counter value.
   procedure Get_Counter (Plugin  : in out Counter_Module;
                          Counter : in AWA.Counters.Counter_Index_Type;
                          Object  : in ADO.Objects.Object_Ref'Class;
                          Result  : out Natural);

   --  Flush the existing counters and update all the database records
   --  refered to them.
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

   --  The `Counter_Map_Array` associate a counter map to each counter definition.
   type Counter_Map_Array is array (Counter_Index_Type range <>) of Counter_Maps.Map;
   type Counter_Map_Array_Access is access all Counter_Map_Array;

   --  Counters are kept temporarily in the `Counter_Table` protected type to avoid
   --  having to update the database each time a counter is incremented.
   --  Counters are flushed when the table reaches some limit, or, when the
   --  table is oldest than some limit.  Counters are associated with a day
   --  so that it becomes possible to gather per-day counters.
   --
   --  The `Flush` operation on the `Counter_Module` can be used to flush
   --  the pending counters.  For each counter that was updated, it either
   --  inserts or updates a row in the counters database table.  Because such
   --  operation is slow, it is not implemented in the protected type.
   --  Instead, we steal the counter table and replace it with a new/empty
   --  table.  This is done by `Steal_Counters` protected operation.
   --  While doing the flush, other tasks can increment counters without
   --  being blocked by the `Flush` operation.
   protected type Counter_Table is

      --  Increment the counter identified by `Counter` and associated with the
      --  database object <tt>Key</tt>.
      procedure Increment (Counter : in Counter_Index_Type;
                           Key     : in ADO.Objects.Object_Key);

      --  Get the counters that have been collected with the date and
      --  prepare to collect new counters.
      procedure Steal_Counters (Result : out Counter_Map_Array_Access;
                                Date   : out Ada.Calendar.Time);

      --  Check if we must flush the counters.
      function Need_Flush (Limit   : in Natural;
                           Seconds : in Duration) return Boolean;

      --  Get the definition ID associated with the counter.
      procedure Get_Definition (Session : in out ADO.Sessions.Master_Session;
                                Counter : in Counter_Index_Type;
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
