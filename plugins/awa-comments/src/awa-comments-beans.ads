-----------------------------------------------------------------------
--  awa-comments-beans -- Beans for the comments module
--  Copyright (C) 2014, 2015, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;

with ADO;
with ADO.Schemas;
with ADO.Sessions;

with ASF.Helpers.Beans;

with AWA.Comments.Models;
with AWA.Comments.Modules;

--  == Ada Beans ==
--  Several bean types are provided to represent and manage a list of tags.
--  The tag module registers the bean constructors when it is initialized.
--  To use them, one must declare a bean definition in the application XML configuration.
--
--  === Comment_List_Bean ===
--  The `Comment_List_Bean` holds a list of comments and provides operations used by the
--  `awa:tagList` component to add or remove tags within a `h:form` component.
--  A bean can be declared and configured as follows in the XML application configuration file:
--
--    <managed-bean>
--      <managed-bean-name>postCommentList</managed-bean-name>
--      <managed-bean-class>AWA.Comments.Beans.Comment_List_Bean</managed-bean-class>
--      <managed-bean-scope>request</managed-bean-scope>
--      <managed-property>
--        <property-name>entity_type</property-name>
--        <property-class>String</property-class>
--        <value>awa_post</value>
--      </managed-property>
--      <managed-property>
--        <property-name>permission</property-name>
--        <property-class>String</property-class>
--        <value>blog-comment-post</value>
--      </managed-property>
--      <managed-property>
--        <property-name>sort</property-name>
--        <property-class>String</property-class>
--        <value>oldest</value>
--      </managed-property>
--      <managed-property>
--        <property-name>status</property-name>
--        <property-class>String</property-class>
--        <value>published</value>
--      </managed-property>
--    </managed-bean>
--
--  The `entity_type` property defines the name of the database table to which the comments
--  are assigned.  The `permission` property defines the permission name that must be used
--  to verify that the user has the permission do add or remove the comment.
--
--  @include-bean comment-queries.xml
package AWA.Comments.Beans is

   type Comment_Bean is new AWA.Comments.Models.Comment_Bean with record
      Module      : Modules.Comment_Module_Access := null;
      Entity_Type : Ada.Strings.Unbounded.Unbounded_String;
      Permission  : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Comment_Bean_Access is access all Comment_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Comment_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Comment_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create the comment.
   overriding
   procedure Create (Bean    : in out Comment_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Save the comment.
   overriding
   procedure Save (Bean    : in out Comment_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Publish or not the comment.
   overriding
   procedure Publish (Bean    : in out Comment_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Delete the comment.
   overriding
   procedure Delete (Bean    : in out Comment_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create a new comment bean instance.
   function Create_Comment_Bean (Module : in AWA.Comments.Modules.Comment_Module_Access)
                                 return Util.Beans.Basic.Readonly_Bean_Access;

   type Comment_List_Bean is
     new AWA.Comments.Models.Comment_Info_List_Bean and Util.Beans.Basic.Bean with record
      Module       : AWA.Comments.Modules.Comment_Module_Access;
      Entity_Type  : Ada.Strings.Unbounded.Unbounded_String;
      Entity_Id    : ADO.Identifier;
      Permission   : Ada.Strings.Unbounded.Unbounded_String;
      Current      : Natural := 0;
      Oldest_First : Boolean := True;
      Publish_Only : Boolean := True;
   end record;

   type Comment_List_Bean_Access is access all Comment_List_Bean'Class;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Comment_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Set the entity type (database table) with which the comments are associated.
   procedure Set_Entity_Type (Into  : in out Comment_List_Bean;
                              Table : in ADO.Schemas.Class_Mapping_Access);

   --  Set the permission to check before removing or adding a comment on the entity.
   procedure Set_Permission (Into       : in out Comment_List_Bean;
                             Permission : in String);

   --  Load the comments associated with the given database identifier.
   procedure Load_Comments (Into          : in out Comment_List_Bean;
                            For_Entity_Id : in ADO.Identifier);

   --  Load the comments associated with the given database identifier.
   procedure Load_Comments (Into          : in out Comment_List_Bean;
                            Session       : in out ADO.Sessions.Session;
                            For_Entity_Id : in ADO.Identifier);

   --  Create the comment list bean instance.
   function Create_Comment_List_Bean (Module : in AWA.Comments.Modules.Comment_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access;

   function Get_Comment_Bean is
     new ASF.Helpers.Beans.Get_Bean (Element_Type   => Comment_Bean,
                                     Element_Access => Comment_Bean_Access);

   function Get_Comment_List_Bean is
     new ASF.Helpers.Beans.Get_Bean (Element_Type   => Comment_List_Bean,
                                     Element_Access => Comment_List_Bean_Access);

end AWA.Comments.Beans;
