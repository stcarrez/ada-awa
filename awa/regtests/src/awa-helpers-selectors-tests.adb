-----------------------------------------------------------------------
--  awa-helpers-selectors-tests -- Unit tests for selector helpers
--  Copyright (C) 2011, 2012, 2013, 2022 Stephane Carrez
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

with Util.Test_Caller;
with AWA.Tests;

with ADO.Sessions;
package body AWA.Helpers.Selectors.Tests is

   package Caller is new Util.Test_Caller (Test, "Helpers.Selectors");

   function Create_From_Color is new Create_From_Enum (Color, "color_");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Helpers.Selectors.Create_From_Query",
                       Test_Create_From_Query'Access);
      Caller.Add_Test (Suite, "Test AWA.Helpers.Selectors.Create_From_Enum",
                       Test_Create_From_Enum'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of selector from an SQL query
   --  ------------------------------
   procedure Test_Create_From_Query (T : in out Test) is
      Session : constant ADO.Sessions.Session := AWA.Tests.Get_Application.Get_Session;
      Query   : constant String := "SELECT id, name from ado_entity_type order by id";
      Stmt    : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
      Result  : ASF.Models.Selects.Select_Item_List;
      Found_User : Boolean := False;
   begin
      Append_From_Query (Result, Stmt);
      T.Assert (Result.Length > 0, "The list should not be empty");

      for I in 1 .. Result.Length loop
         declare
            Item : constant ASF.Models.Selects.Select_Item := Result.Get_Select_Item (I);
         begin
            --  The SQL query will return two different columns.
            --  Just check that label and values are different.
            T.Assert (Item.Get_Value /= Item.Get_Label, "Item and label are equals");

            --  To make this test simple, check only for one known entry in the list.
            if Item.Get_Label = "awa_user" then
               Found_User := True;
            end if;
         end;
      end loop;
      T.Assert (Found_User, "The 'user' entity_type was not found in the selector list");
   end Test_Create_From_Query;

   --  ------------------------------
   --  Test creation of selector from an enum definition
   --  ------------------------------
   procedure Test_Create_From_Enum (T : in out Test) is
      Bundle  : Util.Properties.Bundles.Manager;
      Result  : constant ASF.Models.Selects.Select_Item_List := Create_From_Color (Bundle);
   begin
      T.Assert (Result.Length > 0, "The list should not be empty");
   end Test_Create_From_Enum;

end AWA.Helpers.Selectors.Tests;
