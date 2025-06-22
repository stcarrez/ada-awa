-----------------------------------------------------------------------
--  awa-tags-beans -- Beans for the tags module
--  Copyright (C) 2013, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Finalization;

with Util.Beans.Basic;
with Util.Beans.Objects.Lists;
with Util.Beans.Lists.Strings;
with Util.Strings.Vectors;

with ADO;
with ADO.Utils;
with ADO.Schemas;
with ADO.Sessions;

with AWA.Tags.Models;
with AWA.Tags.Modules;

--  == Ada Beans ==
--  Several bean types are provided to represent and manage a list of tags.
--  The tag module registers the bean constructors when it is initialized.
--  To use them, one must declare a bean definition in the application XML configuration.
--
--  === Tag_List_Bean ===
--  The `Tag_List_Bean` holds a list of tags and provides operations used by the
--  `awa:tagList` component to add or remove tags within a `h:form` component.
--  A bean can be declared and configured as follows in the XML application configuration file:
--
--    <managed-bean>
--      <managed-bean-name>questionTags</managed-bean-name>
--      <managed-bean-class>AWA.Tags.Beans.Tag_List_Bean</managed-bean-class>
--      <managed-bean-scope>request</managed-bean-scope>
--      <managed-property>
--        <property-name>entity_type</property-name>
--        <property-class>String</property-class>
--        <value>awa_question</value>
--      </managed-property>
--      <managed-property>
--        <property-name>permission</property-name>
--        <property-class>String</property-class>
--        <value>question-edit</value>
--      </managed-property>
--    </managed-bean>
--
--  The `entity_type` property defines the name of the database table to which the tags
--  are assigned.  The `permission` property defines the permission name that must be used
--  to verify that the user has the permission do add or remove the tag.  Such permission is
--  verified only when the `awa:tagList` component is used within a form.
--
--  === Tag_Search_Bean ===
--  The `Tag_Search_Bean` is dedicated to searching for tags that start with a given
--  pattern.  The auto complete feature of the `awa:tagList` component can use this
--  bean type to look in the database for tags matching a start pattern.  The declaration of the
--  bean should define the database table to search for tags associated with a given database
--  table.  This is done in the XML configuration with the `entity_type` property.
--
--    <managed-bean>
--      <managed-bean-name>questionTagSearch</managed-bean-name>
--      <managed-bean-class>AWA.Tags.Beans.Tag_Search_Bean</managed-bean-class>
--      <managed-bean-scope>request</managed-bean-scope>
--      <managed-property>
--        <property-name>entity_type</property-name>
--        <property-class>String</property-class>
--        <value>awa_question</value>
--      </managed-property>
--    </managed-bean>
--
--  === Tag_Info_List_Bean ===
--  The <tt>Tag_Info_List_Bean</tt> holds a collection of tags with their weight.  It is used
--  by the <tt>awa:tagCloud</tt> component.
--
--    <managed-bean>
--      <managed-bean-name>questionTagList</managed-bean-name>
--      <managed-bean-class>AWA.Tags.Beans.Tag_Info_List_Bean</managed-bean-class>
--      <managed-bean-scope>request</managed-bean-scope>
--      <managed-property>
--        <property-name>entity_type</property-name>
--        <property-class>String</property-class>
--        <value>awa_question</value>
--      </managed-property>
--    </managed-bean>
--
--  @include-bean tag-queries.xml
package AWA.Tags.Beans is

   --  Compare two tags on their count and name.
   function "<" (Left, Right : in AWA.Tags.Models.Tag_Info) return Boolean;

   package Tag_Ordered_Sets is
     new Ada.Containers.Ordered_Sets (Element_Type => AWA.Tags.Models.Tag_Info,
                                      "="          => AWA.Tags.Models."=");

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

   --  The <tt>Entity_Tag_Map</tt> contains a list of tags associated with some entities.
   --  It allows to retrieve from the database all the tags associated with several entities
   --  and get the list of tags for a given entity.
   type Entity_Tag_Map is new Ada.Finalization.Limited_Controlled with private;

   --  Get the list of tags associated with the given entity.
   --  Returns null if the entity does not have any tag.
   function Get_Tags (From       : in Entity_Tag_Map;
                      For_Entity : in ADO.Identifier)
                      return Util.Beans.Lists.Strings.List_Bean_Access;

   --  Get the list of tags associated with the given entity.
   --  Returns a null object if the entity does not have any tag.
   function Get_Tags (From       : in Entity_Tag_Map;
                      For_Entity : in ADO.Identifier)
                      return Util.Beans.Objects.Object;

   --  Release the list of tags.
   procedure Clear (List : in out Entity_Tag_Map);

   --  Load the list of tags associated with a list of entities.
   procedure Load_Tags (Into        : in out Entity_Tag_Map;
                        Session     : in out ADO.Sessions.Session'Class;
                        Entity_Type : in String;
                        List        : in ADO.Utils.Identifier_Vector);

   --  Release the list of tags.
   overriding
   procedure Finalize (List : in out Entity_Tag_Map);

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

   package Entity_Tag_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type        => ADO.Identifier,
                                     Element_Type    => Util.Beans.Lists.Strings.List_Bean_Access,
                                     Hash            => ADO.Utils.Hash,
                                     Equivalent_Keys => ADO."=",
                                     "="             => Util.Beans.Lists.Strings."=");

   type Entity_Tag_Map is new Ada.Finalization.Limited_Controlled with record
      Tags : Entity_Tag_Maps.Map;
   end record;

end AWA.Tags.Beans;
