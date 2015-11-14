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
with ADO.Sessions.Entities;
with ADO.SQL;

with Util.Dates;
with Util.Log.Loggers;

with AWA.Services.Contexts;
with AWA.Counters.Models;
with AWA.Modules.Get;
package body AWA.Counters.Modules is

   use type ADO.Schemas.Class_Mapping_Access;

   package ASC renames AWA.Services.Contexts;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Counters.Module");

   procedure Load_Definition (DB     : in out ADO.Sessions.Master_Session;
                              Def    : in Counter_Def;
                              Result : out Natural);

   procedure Flush (DB       : in out ADO.Sessions.Master_Session;
                    Counter  : in Counter_Def;
                    Def_Id   : in Natural;
                    Counters : in Counter_Maps.Map;
                    Date     : in Ada.Calendar.Time);

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

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Counter_Module;
                        Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);
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
                              Element : in out Positive);

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

      --  ------------------------------
      --  Get the definition ID associated with the counter.
      --  ------------------------------
      procedure Get_Definition (Counter : in Counter_Index_Type;
                                Result  : out Natural) is
      begin
         if Definitions = null then
            Definitions := new Definition_Array_Type (1 .. Counter_Arrays.Get_Last);
            Definitions.all := (others => 0);
         end if;
         if Definitions (Counter) = 0 then
            declare
               Ctx     : constant ASC.Service_Context_Access := ASC.Current;
               Session : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
            begin
               Load_Definition (Session, Counter_Arrays.Get_Element (Counter).all,
                                Definitions (Counter));
            end;
         end if;
         Result := Definitions (Counter);
      end Get_Definition;

   end Counter_Table;

   procedure Load_Definition (DB     : in out ADO.Sessions.Master_Session;
                              Def    : in Counter_Def;
                              Result : out Natural) is

      Def_Db : AWA.Counters.Models.Counter_Definition_Ref;
      Query  : ADO.SQL.Query;
      Found  : Boolean;
   begin
      if Def.Table = null then
         Query.Set_Filter ("name = :name AND entity_type IS NULL");
      else
         Query.Set_Filter ("name = :name AND entity_type = :entity_type");
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "entity_type",
                                           Table   => Def.Table,
                                           Session => DB);
      end if;
      Query.Bind_Param ("name", Def.Field.all);
      Def_Db.Find (DB, Query, Found);
      if not Found then
         if Def.Table /= null then
            Def_Db.Set_Entity_Type (ADO.Sessions.Entities.Find_Entity_Type (DB, Def.Table));
         end if;
         Def_Db.Set_Name (Def.Field.all);
         Def_Db.Save (DB);
      end if;
      Result := Natural (Def_Db.Get_Id);
   end Load_Definition;

   procedure Flush (DB       : in out ADO.Sessions.Master_Session;
                    Counter  : in Counter_Def;
                    Def_Id   : in Natural;
                    Counters : in Counter_Maps.Map;
                    Date     : in Ada.Calendar.Time) is
      Query  : ADO.Queries.Context;
      Stmt   : ADO.Statements.Query_Statement;
      Update : ADO.Statements.Query_Statement;
      Iter   : Counter_Maps.Cursor := Counters.First;
      Id     : ADO.Identifier;
   begin
      Query.Set_Query (AWA.Counters.Models.Query_Counter_Update);
      Stmt := DB.Create_Statement (Query);
      if Counter.Table /= null then
         Query.Set_Query (AWA.Counters.Models.Query_Counter_Update_Field);
         Update := DB.Create_Statement (Counter.Table);
         Update.Bind_Param ("table", Counter.Table.Table.all);
         Update.Bind_Param ("field", Counter.Field.all);
      end if;
      while Counter_Maps.Has_Element (Iter) loop
         Id := ADO.Objects.Get_Value (Counter_Maps.Key (Iter));
         Stmt.Bind_Param ("date", Date);
         Stmt.Bind_Param ("id", Id);
         Stmt.Bind_Param ("counter", Counter_Maps.Element (Iter));
         Stmt.Bind_Param ("definition", Integer (Def_Id));
         Stmt.Execute;
         if Counter.Table /= null then
            Update.Bind_Param ("counter", Counter_Maps.Element (Iter));
            Update.Bind_Param ("id", Id);
            Update.Execute;
         end if;
         Counter_Maps.Next (Iter);
      end loop;
   end Flush;

   --  ------------------------------
   --  Flush the existing counters and update all the database records refered to them.
   --  ------------------------------
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
         DB     : ADO.Sessions.Master_Session := Plugin.Get_Master_Session;
         Date   : constant Ada.Calendar.Time := Util.Dates.Get_Day_Start (Day);
         Def_Id : Natural;
      begin
         DB.Begin_Transaction;
         for I in Counters'Range loop
            if not Counters (I).Is_Empty then
               declare
                  Counter : constant Counter_Def := Counter_Arrays.Get_Element (I).all;
               begin
                  Plugin.Counters.Get_Definition (I, Def_Id);
                  Flush (DB, Counter, Def_Id, Counters (I), Date);
               end;
            end if;
         end loop;
         DB.Commit;
      end;
      Free (Counters);
   end Flush;

end AWA.Counters.Modules;
