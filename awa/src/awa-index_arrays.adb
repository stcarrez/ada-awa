-----------------------------------------------------------------------
--  awa-index_arrays -- Static index arrays
--  Copyright (C) 2015, 2022 Stephane Carrez
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
with Util.Log.Loggers;
with Ada.Unchecked_Deallocation;

package body AWA.Index_Arrays is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Index_Arrays");

   type Name_Pair is record
      Name  : Element_Type_Access := null;
      Index : Index_Type          := Index_Type'First;
   end record;

   type Name_Pair_Array is array (Index_Type range <>) of Name_Pair;
   type Name_Pair_Array_Access is access all Name_Pair_Array;

   type Name_Array is array (Index_Type range <>) of Element_Type_Access;
   type Name_Array_Access is access all Name_Array;

   --  Register a definition by its name and allocate a unique runtime index.
   procedure Add_Index (Name  : in Element_Type_Access;
                        Index : out Index_Type);

   --  A static list of names.  This array is created during the elaboration
   --  of definitions.  It is sorted on names.
   Indexes    : Name_Pair_Array_Access;

   --  A static list of names indexed by the index.
   Names      : Name_Array_Access;

   --  The index of the last definition.
   Last_Index : Index_Type := Index_Type'First;

   --  ------------------------------
   --  Register a definition by its name and allocate a unique runtime index.
   --  ------------------------------
   procedure Add_Index (Name  : in Element_Type_Access;
                        Index : out Index_Type) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Name_Pair_Array,
                                         Name   => Name_Pair_Array_Access);
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Name_Array,
                                         Name   => Name_Array_Access);

      Left    : Index_Type := Index_Type'First + 1;
      Right   : Index_Type := Last_Index;
   begin
      --  Check the storage and allocate it if necessary.
      if Indexes = null then
         Indexes := new Name_Pair_Array (Index_Type'First + 1 .. Index_Type'First + 10);
         Names  := new Name_Array (Index_Type'First + 1 .. Index_Type'First + 10);
      elsif Indexes'Last = Last_Index then
         declare
            E : constant Name_Pair_Array_Access
              := new Name_Pair_Array (1 .. Last_Index + 10);
            N : constant Name_Array_Access := new Name_Array (1 .. Last_Index + 10);
         begin
            E (Indexes'Range) := Indexes.all;
            N (Names'Range) := Names.all;
            Free (Indexes);
            Free (Names);
            Names := N;
            Indexes := E;
         end;
      end if;

      --  Find the definition position within the sorted table.
      --  If a definition is already registered, bark and re-use the previous index.
      while Left <= Right loop
         declare
            Pos  : constant Index_Type := (Left + Right + 1) / 2;
            Item : constant Element_Type_Access := Indexes (Pos).Name;
         begin
            if Name.all = Item.all then
               Log.Error ("Definition " & Name.all & " is already registered.");
               Index := Indexes (Pos).Index;
               return;
            elsif Name.all < Item.all then
               Right := Pos - 1;
            else
               Left := Pos + 1;
            end if;
         end;
      end loop;

      --  Insert the new definition at the good position now.
      if Left = 0 then
         Left := Left + 1;
      end if;
      if Left <= Last_Index then
         Indexes (Left + 1 .. Last_Index + 1) := Indexes (Left .. Last_Index);

      end if;
      Last_Index := Last_Index + 1;
      Indexes (Left).Name := Name;
      Indexes (Left).Index := Last_Index;
      Names (Last_Index) := Name;
      Index := Last_Index;
      Log.Debug ("Definition " & Name.all & " index is {0}", Index_Type'Image (Index));
   end Add_Index;

   --  ------------------------------
   --  Declare a new definition
   --  ------------------------------
   package body Definition is
      Index : Index_Type;

      Index_Name : aliased constant Element_Type := Name;

      function Kind return Index_Type is
      begin
         return Index;
      end Kind;

   begin
      Add_Index (Index_Name'Access, Index);
   end Definition;

   --  ------------------------------
   --  Find the runtime index given the name.
   --  Raises Not_Found exception if the name is not recognized.
   --  ------------------------------
   function Find (Name : in Element_Type) return Index_Type is
      Left    : Index_Type := 1;
      Right   : Index_Type := Last_Index;
   begin
      while Left <= Right loop
         declare
            Pos  : constant Index_Type := (Left + Right + 1) / 2;
            Item : constant Element_Type_Access := Indexes (Pos).Name;
         begin
            if Name = Item.all then
               return Indexes (Pos).Index;
            elsif Name < Item.all then
               Right := Pos - 1;
            else
               Left := Pos + 1;
            end if;
         end;
      end loop;
      Log.Error ("Definition " & Name & " not recognized.");
      raise Not_Found with "Definition " & Name & " not found";
   end Find;

   --  ------------------------------
   --  Get the element associated with the index.
   --  ------------------------------
   function Get_Element (Index : in Index_Type) return Element_Type_Access is
   begin
      if Index = Invalid_Index or else Index > Last_Index then
         Log.Error ("Index {0} is out of bounds", Index_Type'Image (Index));
         raise Not_Found;
      end if;
      return Names (Index);
   end Get_Element;

   --  ------------------------------
   --  Check if the index is a valid index.
   --  ------------------------------
   function Is_Valid (Index : in Index_Type) return Boolean is
   begin
      return Index > Invalid_Index and then Index <= Last_Index;
   end Is_Valid;

   --  ------------------------------
   --  Get the last valid index.
   --  ------------------------------
   function Get_Last return Index_Type is
   begin
      return Last_Index;
   end Get_Last;

end AWA.Index_Arrays;
