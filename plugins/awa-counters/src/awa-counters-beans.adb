-----------------------------------------------------------------------
--  awa-counters-beans -- Counter bean definition
--  Copyright (C) 2015, 2016, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Calendar;
with Util.Dates.ISO8601;
with ADO.Queries.Loaders;
with ADO.Sessions.Entities;
with AWA.Services.Contexts;
package body AWA.Counters.Beans is

   package ASC renames AWA.Services.Contexts;

   function Parse_Date (Date : in String;
                        Now  : in Ada.Calendar.Time) return Ada.Calendar.Time;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Counter_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Name);
   begin
      return Util.Beans.Objects.To_Object (From.Value);
   end Get_Value;

   overriding
   function Get_Value (List : in Counter_Stat_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "stats" then
         return Util.Beans.Objects.To_Object (Value   => List.Stats_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      else
         return AWA.Counters.Models.Stat_List_Bean (List).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Counter_Stat_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if not Util.Beans.Objects.Is_Empty (Value) then
         AWA.Counters.Models.Stat_List_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Get the query definition to collect the counter statistics.
   --  ------------------------------
   function Get_Query (From : in Counter_Stat_Bean) return ADO.Queries.Query_Definition_Access is
      Name : constant String := Ada.Strings.Unbounded.To_String (From.Query_Name);
   begin
      return ADO.Queries.Loaders.Find_Query (Name);
   end Get_Query;

   function Parse_Date (Date : in String;
                        Now  : in Ada.Calendar.Time) return Ada.Calendar.Time is
      use type Ada.Calendar.Time;

      Day : Natural;
   begin
      if Date'Length = 0 or else Date = "now" then
         return Now;
      elsif Date'Length > 5 and then Date (Date'First .. Date'First + 3) = "now-" then
         Day := Natural'Value (Date (Date'First + 4 .. Date'Last));
         return Now - Duration (Day * 86400);
      else
         return Util.Dates.ISO8601.Value (Date);
      end if;
   end Parse_Date;

   --  ------------------------------
   --  Load the statistics information.
   --  ------------------------------
   overriding
   procedure Load (List    : in out Counter_Stat_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
      use type ADO.Identifier;
      use type ADO.Queries.Query_Definition_Access;

      Ctx         : constant ASC.Service_Context_Access := ASC.Current;
      User        : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Now         : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Def         : constant ADO.Queries.Query_Definition_Access := List.Get_Query;
      Entity_Type : constant String := Ada.Strings.Unbounded.To_String (List.Entity_Type);
      Session     : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Kind        : ADO.Entity_Type;
      First_Date  : Ada.Calendar.Time;
      Last_Date   : Ada.Calendar.Time;
      Query       : ADO.Queries.Context;
   begin
      Kind := ADO.Sessions.Entities.Find_Entity_Type (Session, Entity_Type);
      First_Date := Parse_Date (Ada.Strings.Unbounded.To_String (List.First_Date), Now);
      Last_Date := Parse_Date (Ada.Strings.Unbounded.To_String (List.Last_Date), Now);
      if List.Entity_Id /= ADO.NO_IDENTIFIER and then Def /= null then
         Query.Set_Query (Def);
         Query.Bind_Param ("entity_type", Kind);
         Query.Bind_Param ("entity_id", List.Entity_Id);
         Query.Bind_Param ("user_id", User);
         Query.Bind_Param ("first_date", First_Date);
         Query.Bind_Param ("last_date", Last_Date);
         Query.Bind_Param ("counter_name", List.Counter_Name);

         AWA.Counters.Models.List (List.Stats, Session, Query);
      end if;
   end Load;

   --  ------------------------------
   --  Create the Blog_Stat_Bean bean instance.
   --  ------------------------------
   function Create_Counter_Stat_Bean (Module : in AWA.Counters.Modules.Counter_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Counter_Stat_Bean_Access := new Counter_Stat_Bean;
   begin
      Object.Module     := Module;
      Object.Stats_Bean := Object.Stats'Access;
      return Object.all'Access;
   end Create_Counter_Stat_Bean;

end AWA.Counters.Beans;
