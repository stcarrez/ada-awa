-----------------------------------------------------------------------
--  awa-helpers -- Helpers for AWA applications
--  Copyright (C) 2011 Stephane Carrez
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

package body AWA.Helpers.Selectors is

   --  ------------------------------
   --  Create a selector list from the definition of a discrete type such as an enum.
   --  The select item has the enum value as value and the label is created by
   --  localizing the string <b>Prefix</b>_<i>enum name</i>.
   --  ------------------------------
   function Create_From_Enum (Bundle : in Util.Properties.Bundles.Manager'Class)
                              return ASF.Models.Selects.Select_Item_List is
      Result : ASF.Models.Selects.Select_Item_List;
   begin
      for I in T'Range loop
         declare
            Value : constant String := T'Image (I);
            Name  : constant String := Prefix & "_" & Value;
            Label : constant String := Bundle.Get (Name, Name);
         begin
            Result.Append (ASF.Models.Selects.Create_Select_Item (Label, Value));
         end;
      end loop;
      return Result;
   end Create_From_Enum;

   --  ------------------------------
   --  Append the selector list from the SQL query.  The query will be executed.
   --  It should return rows with at least two columns.  The first column is the
   --  selector value and the second column is the selector label.
   --  ------------------------------
   procedure Append_From_Query (Into  : in out ASF.Models.Selects.Select_Item_List;
                                Query : in out ADO.Statements.Query_Statement'Class) is
   begin
      Query.Execute;
      while Query.Has_Elements loop
         declare
            Id    : constant String := Query.Get_String (1);
            Label : constant String := Query.Get_String (2);
         begin
            Into.Append (ASF.Models.Selects.Create_Select_Item (Id, Label));
         end;
         Query.Next;
      end loop;
   end Append_From_Query;

   --  ------------------------------
   --  Create the selector list from the SQL query.  The query will be executed.
   --  It should return rows with at least two columns.  The first column is the
   --  selector value and the second column is the selector label.
   --  ------------------------------
   function Create_From_Query (Session : in ADO.Sessions.Session'Class;
                               Query   : in ADO.Queries.Context'Class)
                               return ASF.Models.Selects.Select_Item_List is
      Stmt   : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
      Result : ASF.Models.Selects.Select_Item_List;
   begin
      Append_From_Query (Result, Stmt);
      return Result;
   end Create_From_Query;

end AWA.Helpers.Selectors;
