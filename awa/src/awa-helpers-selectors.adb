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

with ADO.Queries.Loaders;
with AWA.Services.Contexts;
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

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : in Select_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "list" then
         return ASF.Models.Selects.To_Object (From.List);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   procedure Set_Value (From  : in out Select_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
      use type AWA.Services.Contexts.Service_Context_Access;
      use type ADO.Queries.Query_Definition_Access;
   begin
      if Name = "query" then
         declare
            Query_Name : constant String := Util.Beans.Objects.To_String (Value);
            Ctx : constant AWA.Services.Contexts.Service_Context_Access
              := AWA.Services.Contexts.Current;
            Query_Def : constant ADO.Queries.Query_Definition_Access
              := ADO.Queries.Loaders.Find_Query (Query_Name);
            Query     : ADO.Queries.Context;
         begin
            if Ctx = null or Query_Def = null then
               return;
            end if;

            Query.Set_Query (Query_Def);
            From.List := Create_From_Query (Session => AWA.Services.Contexts.Get_Session (Ctx),
                                            Query   => Query);
         end;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create the select list bean instance.
   --  ------------------------------
   function Create_Select_List_Bean return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Select_List_Bean_Access := new Select_List_Bean;
   begin
      return Result.all'Access;
   end Create_Select_List_Bean;

end AWA.Helpers.Selectors;
