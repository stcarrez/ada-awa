-----------------------------------------------------------------------
--  awa-tags-beans -- Beans for the tags module
--  Copyright (C) 2013 Stephane Carrez
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
with ADO.Queries;
with ADO.Statements;
with ADO.Sessions;
with ADO.Sessions.Entities;

with AWA.Tags.Models;
package body AWA.Tags.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Tag_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (Integer (From.List.Length));
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Tag_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "entity_type" then
         From.Entity_Type := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "permission" then
         From.Permission := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : in Tag_List_Bean) return Natural is
   begin
      return Natural (From.List.Length);
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out Tag_List_Bean;
                            Index : in Natural) is
   begin
      From.Current := Index;
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : in Tag_List_Bean) return Util.Beans.Objects.Object is
   begin
      return From.List.Element (From.Current);
   end Get_Row;

   --  ------------------------------
   --  Set the entity type (database table) onto which the tags are associated.
   --  ------------------------------
   procedure Set_Entity_Type (Into  : in out Tag_List_Bean;
                              Table : in ADO.Schemas.Class_Mapping_Access) is
   begin
      Into.Entity_Type := Ada.Strings.Unbounded.To_Unbounded_String (Table.Table.all);
   end Set_Entity_Type;

   --  ------------------------------
   --  Load the tags associated with the given database identifier.
   --  ------------------------------
   procedure Load_Tags (Into          : in out Tag_List_Bean;
                        Session       : in ADO.Sessions.Session;
                        For_Entity_Id : in ADO.Identifier) is
      use ADO.Sessions.Entities;

--        Session     : constant ADO.Sessions.Session := Into.Module.Get_Session;
      Entity_Type : constant String := Ada.Strings.Unbounded.To_String (Into.Entity_Type);
      Kind        : constant ADO.Entity_Type := Find_Entity_Type (Session, Entity_Type);
      Query       : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Tags.Models.Query_Tag_List);
      Query.Bind_Param ("entity_type", Kind);
      Query.Bind_Param ("entity_id", For_Entity_Id);
      declare
         Stmt : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
      begin
         Stmt.Execute;

         while Stmt.Has_Elements loop
            Into.List.Append (Util.Beans.Objects.To_Object (Stmt.Get_String (0)));
            Stmt.Next;
         end loop;
      end;
   end Load_Tags;

   --  ------------------------------
   --  Create the tag list bean instance.
   --  ------------------------------
   function Create_Tag_List_Bean (Module : in AWA.Tags.Modules.Tag_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Tag_List_Bean_Access := new Tag_List_Bean;
   begin
      Result.Module := Module;
      return Result.all'Access;
   end Create_Tag_List_Bean;

end AWA.Tags.Beans;
