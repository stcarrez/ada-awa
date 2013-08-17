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
with Util.Beans.Objects.Lists;
with Util.Strings.Vectors;

with ADO;
with ADO.Schemas;
with ADO.Sessions;

with AWA.Tags.Models;
with AWA.Tags.Modules;
package AWA.Tags.Beans is

   type Tag_List_Bean is
     new Util.Beans.Objects.Lists.List_Bean and Util.Beans.Basic.Bean with private;

   type Tag_List_Bean_Access is access all Tag_List_Bean'Class;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Tag_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Set the entity type (database table) onto which the tags are associated.
   procedure Set_Entity_Type (Into  : in out Tag_List_Bean;
                              Table : in ADO.Schemas.Class_Mapping_Access);

   --  Set the permission to check before removing or adding a tag on the entity.
   procedure Set_Permission (Into       : in out Tag_List_Bean;
                             Permission : in String);

   --  Load the tags associated with the given database identifier.
   procedure Load_Tags (Into          : in out Tag_List_Bean;
                        Session       : in ADO.Sessions.Session;
                        For_Entity_Id : in ADO.Identifier);

   --  Set the list of tags to add.
   procedure Set_Added (Into  : in out Tag_List_Bean;
                        Tags  : in Util.Strings.Vectors.Vector);

   --  Set the list of tags to remove.
   procedure Set_Deleted (Into : in out Tag_List_Bean;
                          Tags : in Util.Strings.Vectors.Vector);

   --  Update the tags associated with the tag entity represented by <tt>For_Entity_Id</tt>.
   --  The list of tags defined by <tt>Set_Deleted</tt> are removed first and the list of
   --  tags defined by <tt>Set_Added</tt> are associated with the database entity.
   procedure Update_Tags (From          : in Tag_List_Bean;
                          For_Entity_Id : in ADO.Identifier);

   --  Create the tag list bean instance.
   function Create_Tag_List_Bean (Module : in AWA.Tags.Modules.Tag_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access;

   type Tag_Search_Bean is
     new Util.Beans.Objects.Lists.List_Bean and Util.Beans.Basic.Bean with private;
   type Tag_Search_Bean_Access is access all Tag_Search_Bean'Class;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Tag_Search_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Search the tags that match the search string.
   procedure Search_Tags (Into    : in out Tag_Search_Bean;
                          Session : in ADO.Sessions.Session;
                          Search  : in String);

   --  Set the entity type (database table) onto which the tags are associated.
   procedure Set_Entity_Type (Into  : in out Tag_Search_Bean;
                              Table : in ADO.Schemas.Class_Mapping_Access);

   --  Create the tag search bean instance.
   function Create_Tag_Search_Bean (Module : in AWA.Tags.Modules.Tag_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access;

   type Tag_Info_List_Bean is
     new AWA.Tags.Models.Tag_Info_List_Bean and Util.Beans.Basic.Bean with private;
   type Tag_Info_List_Bean_Access is access all Tag_Info_List_Bean'Class;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Tag_Info_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Load the list of tags.
   procedure Load_Tags (Into    : in out Tag_Info_List_Bean;
                        Session : in out ADO.Sessions.Session);

   --  Create the tag info list bean instance.
   function Create_Tag_Info_List_Bean (Module : in AWA.Tags.Modules.Tag_Module_Access)
                                       return Util.Beans.Basic.Readonly_Bean_Access;

private

   type Tag_List_Bean is
     new Util.Beans.Objects.Lists.List_Bean and Util.Beans.Basic.Bean with record
      Module      : AWA.Tags.Modules.Tag_Module_Access;
      Entity_Type : Ada.Strings.Unbounded.Unbounded_String;
      Permission  : Ada.Strings.Unbounded.Unbounded_String;
      Current     : Natural := 0;
      Added       : Util.Strings.Vectors.Vector;
      Deleted     : Util.Strings.Vectors.Vector;
   end record;

   type Tag_Search_Bean is
     new Util.Beans.Objects.Lists.List_Bean and Util.Beans.Basic.Bean with record
      Module      : AWA.Tags.Modules.Tag_Module_Access;
      Entity_Type : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Tag_Info_List_Bean is
     new AWA.Tags.Models.Tag_Info_List_Bean and Util.Beans.Basic.Bean with record
      Module      : AWA.Tags.Modules.Tag_Module_Access;
      Entity_Type : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end AWA.Tags.Beans;
