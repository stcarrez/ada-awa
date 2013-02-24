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
