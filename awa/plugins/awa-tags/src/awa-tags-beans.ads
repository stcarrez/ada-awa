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
with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects.Vectors;

with ADO;
with ADO.Schemas;
with ADO.Sessions;

with AWA.Tags.Modules;
package AWA.Tags.Beans is

   type Tag_List_Bean is new Util.Beans.Basic.List_Bean and Util.Beans.Basic.Bean with private;

   type Tag_List_Bean_Access is access all Tag_List_Bean'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Tag_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Tag_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : in Tag_List_Bean) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out Tag_List_Bean;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in Tag_List_Bean) return Util.Beans.Objects.Object;

   --  Set the entity type (database table) onto which the tags are associated.
   procedure Set_Entity_Type (Into  : in out Tag_List_Bean;
                              Table : in ADO.Schemas.Class_Mapping_Access);

   --  Load the tags associated with the given database identifier.
   procedure Load_Tags (Into          : in out Tag_List_Bean;
                        Session       : in ADO.Sessions.Session;
                        For_Entity_Id : in ADO.Identifier);

   --  Create the tag list bean instance.
   function Create_Tag_List_Bean (Module : in AWA.Tags.Modules.Tag_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access;

private

   type Tag_List_Bean is new Util.Beans.Basic.List_Bean and Util.Beans.Basic.Bean with record
      List        : Util.Beans.Objects.Vectors.Vector;
      Module      : AWA.Tags.Modules.Tag_Module_Access;
      Entity_Type : Ada.Strings.Unbounded.Unbounded_String;
      Permission  : Ada.Strings.Unbounded.Unbounded_String;
      Current     : Natural := 0;
   end record;

end AWA.Tags.Beans;
