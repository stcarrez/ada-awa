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
with Ada.Unchecked_Deallocation;

with ADO.Queries;
with ADO.Statements;
with ADO.Sessions;

with Util.Dates;
with Util.Log.Loggers;

with AWA.Counters.Models;
with AWA.Modules.Get;
package body AWA.Counters.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Counters.Module");

   --  ------------------------------
   --  Initialize the counters module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Counter_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the counters module");

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  Configures the module after its initialization and after having read its XML configuration.
   overriding
   procedure Configure (Plugin : in out Counter_Module;
                        Props  : in ASF.Applications.Config) is
   begin
      Plugin.Counter_Limit := Plugin.Get_Config (PARAM_COUNTER_LIMIT, DEFAULT_COUNTER_LIMIT);
      Plugin.Age_Limit := Duration (Plugin.Get_Config (PARAM_AGE_LIMIT, 300));
   end Configure;

   --  ------------------------------
   --  Get the counters module.
   --  ------------------------------
   function Get_Counter_Module return Counter_Module_Access is
      function Get is new AWA.Modules.Get (Counter_Module, Counter_Module_Access, NAME);
   begin
      return Get;
   end Get_Counter_Module;

   --  ------------------------------
   --  Increment the counter identified by <tt>Counter</tt> and associated with the
   --  database object <tt>Object</tt>.
   --  ------------------------------
   procedure Increment (Plugin  : in out Counter_Module;
                        Counter : in Counter_Index_Type;
                        Object  : in ADO.Objects.Object_Ref'Class) is
   begin
      if Plugin.Counters.Need_Flush (Plugin.Counter_Limit, Plugin.Age_Limit) then
         Plugin.Flush;
      end if;
      Plugin.Counters.Increment (Counter, Object);
   end Increment;

   protected body Counter_Table is

      --  ------------------------------
      --  Increment the counter identified by <tt>Counter</tt> and associated with the
      --  database object <tt>Object</tt>.
      --  ------------------------------
      procedure Increment (Counter : in Counter_Index_Type;
                           Object  : in ADO.Objects.Object_Ref'Class) is

         procedure Increment (Key     : in ADO.Objects.Object_Key;
                              Element : in out Positive) is
            pragma Unreferenced (Key);
         begin
            Element := Element + 1;
         end Increment;

         Key : constant ADO.Objects.Object_Key := Object.Get_Key;
         Pos : Counter_Maps.Cursor;
      begin
         if Counters = null then
            Counters := new Counter_Map_Array (1 .. Counter_Arrays.Get_Last);
            Day := Ada.Calendar.Clock;
            Day_End := Util.Dates.Get_Day_End (Day);
         end if;
         Pos := Counters (Counter).Find (Key);
         if Counter_Maps.Has_Element (Pos) then
            Counters (Counter).Update_Element (Pos, Increment'Access);
         else
            Counters (Counter).Insert (Key, 1);
            Nb_Counters := Nb_Counters + 1;
         end if;
      end Increment;

      --  ------------------------------
      --  Get the counters that have been collected with the date and prepare to collect
      --  new counters.
      --  ------------------------------
      procedure Steal_Counters (Result : out Counter_Map_Array_Access;
                                Date   : out Ada.Calendar.Time) is
      begin
         Result      := Counters;
         Date        := Day;
         Counters    := null;
         Nb_Counters := 0;
      end Steal_Counters;

      --  ------------------------------
      --  Check if we must flush the counters.
      --  ------------------------------
      function Need_Flush (Limit   : in Natural;
                           Seconds : in Duration) return Boolean is
         use type Ada.Calendar.Time;
      begin
         if Counters = null then
            return False;
         elsif Nb_Counters > Limit then
            return True;
         else
            declare
               Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            begin
               return Now > Day_End or Now - Day >= Seconds;
            end;
         end if;
      end Need_Flush;

   end Counter_Table;

   procedure Flush (DB       : in out ADO.Sessions.Master_Session;
                    Def_Id   : in Counter_Index_Type;
                    Counters : in Counter_Maps.Map;
                    Date     : in Ada.Calendar.Time) is
      Query : ADO.Queries.Context;
      Stmt  : ADO.Statements.Query_Statement;
      Iter  : Counter_Maps.Cursor := Counters.First;
      Id    : ADO.Identifier;
   begin
      Query.Set_Query (AWA.Counters.Models.Query_Counter_Update);
      Stmt := DB.Create_Statement (Query);
      while Counter_Maps.Has_Element (Iter) loop
         Id := ADO.Objects.Get_Value (Counter_Maps.Key (Iter));
         Stmt.Bind_Param ("date", Date);
         Stmt.Bind_Param ("id", Id);
         Stmt.Bind_Param ("counter", Counter_Maps.Element (Iter));
         Stmt.Bind_Param ("definition", Integer (Def_Id));
         Stmt.Execute;
         Counter_Maps.Next (Iter);
      end loop;
   end Flush;

   --  Flush the existing counters and update all the database records refered to them.
   procedure Flush (Plugin : in out Counter_Module) is
      procedure Free is new
        Ada.Unchecked_Deallocation (Object => Counter_Map_Array,
                                    Name   => Counter_Map_Array_Access);

      Day      : Ada.Calendar.Time;
      Counters : Counter_Map_Array_Access;
   begin
      Plugin.Counters.Steal_Counters (Counters, Day);
      if Counters = null then
         return;
      end if;
      declare
         DB   : ADO.Sessions.Master_Session := Plugin.Get_Master_Session;
         Date : constant Ada.Calendar.Time := Util.Dates.Get_Day_Start (Day);
      begin
         DB.Begin_Transaction;
         for I in Counters'Range loop
            if not Counters (I).Is_Empty then
               Flush (DB, I, Counters (I), Date);
            end if;
         end loop;
         DB.Commit;
      end;
   end Flush;

end AWA.Counters.Modules;
