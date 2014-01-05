-----------------------------------------------------------------------
--  awa-comments-beans -- Beans for the comments module
--  Copyright (C) 2014 Stephane Carrez
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

with ADO;
with ADO.Schemas;
with ADO.Sessions;

with AWA.Comments.Models;
with AWA.Comments.Modules;

--  == Comment Beans ==
--  Several bean types are provided to represent and manage a list of tags.
--  The tag module registers the bean constructors when it is initialized.
--  To use them, one must declare a bean definition in the application XML configuration.
--
--  === Comment_List_Bean ===
--  The <tt>Comment_List_Bean</tt> holds a list of comments and provides operations used by the
--  <tt>awa:tagList</tt> component to add or remove tags within a <tt>h:form</tt> component.
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
--    </managed-bean>
--
--  The <tt>entity_type</tt> property defines the name of the database table to which the comments
--  are assigned.  The <tt>permission</tt> property defines the permission name that must be used
--  to verify that the user has the permission do add or remove the comment.
--
package AWA.Comments.Beans is

   type Comment_List_Bean is
     new Util.Beans.Objects.Lists.List_Bean and Util.Beans.Basic.Bean with private;

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
                            Session       : in ADO.Sessions.Session;
                            For_Entity_Id : in ADO.Identifier);

   --  Create the comment list bean instance.
   function Create_Comment_List_Bean (Module : in AWA.Comments.Modules.Comment_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access;

private

   type Comment_List_Bean is
     new Util.Beans.Objects.Lists.List_Bean and Util.Beans.Basic.Bean with record
      Module      : AWA.Comments.Modules.Comment_Module_Access;
      Entity_Type : Ada.Strings.Unbounded.Unbounded_String;
      Permission  : Ada.Strings.Unbounded.Unbounded_String;
      Current     : Natural := 0;
   end record;

end AWA.Comments.Beans;
