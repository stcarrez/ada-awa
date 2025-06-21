-----------------------------------------------------------------------
--  import_country -- Read a Country csv file to update the database
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

with ADO;
with ADO.SQL;
with ADO.Drivers;
with ADO.Sessions;
with ADO.Statements;
with ADO.Sessions.Factory;

with Util.Strings;
with Util.Log.Loggers;
with Util.Serialize.IO.CSV;

with AWA.Countries.Models;
procedure Import_Country is

   use Ada.Text_IO;
   use Util.Serialize.IO.CSV;
   use AWA.Countries.Models;
   use Ada.Containers;

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("Import_Country");

   Country : AWA.Countries.Models.Country_Ref;
   DB      : ADO.Sessions.Master_Session;

   package Country_Map is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => AWA.Countries.Models.Country_Ref,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   package Neighbors_Map is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => String,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   Countries : Country_Map.Map;
   Neighbors : Neighbors_Map.Map;

   type CSV_Parser is new Util.Serialize.IO.CSV.Parser with null record;

   overriding
   procedure Set_Cell (Parser : in out CSV_Parser;
                       Value  : in String;
                       Row    : in Util.Serialize.IO.CSV.Row_Type;
                       Column : in Util.Serialize.IO.CSV.Column_Type);

   overriding
   procedure Set_Cell (Parser : in out CSV_Parser;
                       Value  : in String;
                       Row    : in Util.Serialize.IO.CSV.Row_Type;
                       Column : in Util.Serialize.IO.CSV.Column_Type) is
      pragma Unreferenced (Parser, Row);

      Query : ADO.SQL.Query;
      Found : Boolean;
   begin
      case Column is
         when 1 =>
            --  The ISO code is unique, find the country
            Query.Bind_Param (1, Value);
            Query.Set_Filter ("iso_code = ?");
            Country.Find (DB, Query, Found);
            if not Found then
               --  Build a new country object
               Country := AWA.Countries.Models.Null_Country;
               Country.Set_Iso_Code (Value);
            end if;
            Countries.Insert (Value, Country);

         when 2 =>
            --              Ada.Text_IO.Put_Line ("ISO3: " & Value);
            null;

         when 3 =>
            --              Ada.Text_IO.Put_Line ("ISON: " & Value);
            null;

         when 4 =>
            --              Ada.Text_IO.Put_Line ("FIPS: " & Value);
            null;

            --  Country name
         when 5 =>
            Country.Set_Name (Value);

         when 6 =>
            --              Ada.Text_IO.Put_Line ("Capital: " & Value);
            null;

         when 7 | 8 => -- Area, Population
            null;

            --  Country continent
         when 9 =>
            Country.Set_Continent (Value);

            --  Country TLD
         when 10 =>
            Country.Set_Tld (Value);

            --  Country CurrencyCode
         when 11 =>
            Country.Set_Currency_Code (Value);

            --  Country CurrencyName
         when 12 =>
            Country.Set_Currency (Value);

         when 13 | 14 => --  Phone, postal code format
            null;

         when 15 =>
            --              Ada.Text_IO.Put_Line ("Postal regex: " & Value);
            null;

            --  Country languages
         when 16 =>
            Country.Set_Languages (Value);

            --  Country unique geonameid
         when 17 =>
            if Value /= "" then
               Country.Set_Geonameid (Integer'Value (Value));
            end if;

         when 18 =>
            Country.Save (DB);
            Neighbors.Insert (Country.Get_Iso_Code, Value);

         when 19 => --  EquivalentFipsCode
            null;

         when others =>
            null;

      end case;

   exception
      when E : others =>
         Log.Error ("Column " & Util.Serialize.IO.CSV.Column_Type'Image (Column)
                    & " value: " & Value, E, True);
         raise;
   end Set_Cell;

   procedure Build_Neighbors is
      Stmt : ADO.Statements.Delete_Statement
        := DB.Create_Statement (AWA.Countries.Models.COUNTRY_NEIGHBOR_TABLE);
      Iter : Neighbors_Map.Cursor := Neighbors.First;
      Count : Natural := 0;
   begin
      Stmt.Execute;

      while Neighbors_Map.Has_Element (Iter) loop
         declare
            Name : constant String := Neighbors_Map.Key (Iter);
            List : constant String := Neighbors_Map.Element (Iter);
            Pos  : Natural := List'First;
            N    : Natural;
         begin
            Country := Countries.Element (Name);
            while Pos < List'Last loop
               N := Util.Strings.Index (List, ',', Pos);
               if N = 0 then
                  N := List'Last;
               else
                  N := N - 1;
               end if;
               if Pos < N and then Countries.Contains (List (Pos .. N)) then
                  declare
                     Neighbor : AWA.Countries.Models.Country_Neighbor_Ref;
                  begin
                     Neighbor.Set_Neighbor_Of (Country);
                     Neighbor.Set_Neighbor (Countries.Element (List (Pos .. N)));
                     Neighbor.Save (DB);
                     Count := Count + 1;
                  end;
               else
                  Ada.Text_IO.Put_Line ("Country not found: " & List (Pos .. N));
               end if;
               Pos := N + 2;
            end loop;
         end;
         Neighbors_Map.Next (Iter);
      end loop;
      Ada.Text_IO.Put_Line ("Created " & Natural'Image (Count) & " country neighbors");
   end Build_Neighbors;

   Count   : constant Natural := Ada.Command_Line.Argument_Count;
   Factory : ADO.Sessions.Factory.Session_Factory;
   Parser  : CSV_Parser;
begin
   if Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: import_country config file");
      return;
   end if;

   declare
      Config : constant String := Ada.Command_Line.Argument (1);
   begin
      Util.Log.Loggers.Initialize (Config);

      --  Initialize the database drivers.
      ADO.Drivers.Initialize (Config);

      --  Initialize the session factory to connect to the
      --  database defined by 'ado.database' property.
      Factory.Create (ADO.Drivers.Get_Config ("database"));
   end;

   DB := Factory.Get_Master_Session;
   DB.Begin_Transaction;

   declare
      File : constant String := Ada.Command_Line.Argument (2);
   begin
      Parser.Set_Comment_Separator ('#');
      Parser.Set_Field_Separator (ASCII.HT);
      Parser.Parse (File);

      Ada.Text_IO.Put_Line ("Found " & Count_Type'Image (Countries.Length) & " countries");
      Build_Neighbors;
   end;
   DB.Commit;
end Import_Country;
