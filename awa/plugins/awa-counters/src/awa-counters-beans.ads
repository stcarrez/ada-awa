-----------------------------------------------------------------------
--  awa-counters-beans -- Counter bean definition
--  Copyright (C) 2015, 2016, 2020 Stephane Carrez
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

with ADO.Objects;
with ADO.Schemas;
with ADO.Queries;
with Util.Beans.Objects;
with Util.Beans.Basic;

with AWA.Counters.Modules;
with AWA.Counters.Models;

--  == Ada Bean ==
--  The `Counter_Bean` allows to represent a counter associated with some database
--  entity and allows its control by the `<awa:counter>` HTML component.
--  To use it, an instance of the `Counter_Bean` should be defined in a another
--  Ada bean declaration and configured.  For example, it may be declared
--  as follows:
--
--    type Wiki_View_Bean is new AWA.Wikis.Models.Wiki_View_Info
--    with record
--      ...
--      Counter : aliased Counter_Bean
--         (Of_Type => ADO.Objects.KEY_INTEGER,
--          Of_Class => AWA.Wikis.Models.WIKI_PAGE_TABLE);
--    end record;
--
--  The counter value is held by the `Value` member of `Counter_Bean` and
--  it should be initialized programatically when the Ada bean instance
--  is loaded (for example through a `load` action).
--  The `Counter_Bean` needs to know the database entity to which it
--  is associated and its `Object` member must be initialized.
--  This is necessary for the `<awa:counter>` HTML component to increment
--  the associated counter when the page is displayed.
--  Below is an extract of such initialization:
--
--    procedure Load
--      (Bean    : in out Wiki_View_Bean;
--       Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
--    begin
--      ...
--      Bean.Counter.Value := Bean.Get_Read_Count;
--      ADO.Objects.Set_Value (Bean.Counter.Object, Bean.Get_Id);
--    end Load;
--
--
--  The `Stat_List_Bean` allows to retrieve the list of counters per day for
--  a given database entity.  It needs a special managed bean configuration
--  that describes the database entity type, the counter name and
--  SQL query name.
--
--  The example below from the [Wikis Module] declares the bean
--  `wikiPageStats`.  The database entity is `awa_wiki_page` which is the
--  name of the database table that holds wiki page.  The SQL query
--  to retrieve the result is `page-access-stats`.
--
--   <managed-bean>
--    <description>The counter statistics for a wiki page</description>
--    <managed-bean-name>wikiPageStats</managed-bean-name>
--    <managed-bean-class>AWA.Counters.Beans.Stat_List_Bean</managed-bean-class>
--    <managed-bean-scope>request</managed-bean-scope>
--    <managed-property>
--      <property-name>entity_type</property-name>
--      <property-class>String</property-class>
--      <value>awa_wiki_page</value>
--    </managed-property>
--    <managed-property>
--      <property-name>counter_name</property-name>
--      <property-class>String</property-class>
--      <value>read_count</value>
--    </managed-property>
--    <managed-property>
--      <property-name>query_name</property-name>
--      <property-class>String</property-class>
--      <value>page-access-stats</value>
--    </managed-property>
--   </managed-bean>
--
--  A typical XHTML view that wants to use such bean, should call the `load`
--  action at beginning to load the counter statistics by running the SQL
--  query.
--
--    <f:view contentType="application/json; charset=UTF-8"
--            xmlns:f="http://java.sun.com/jsf/core"
--            xmlns:h="http://java.sun.com/jsf/html">
--      <f:metadata>
--        <f:viewAction action='#{wikiPageStats.load}'/>
--      </f:metadata>
--    {"data":[<h:list value="#{wikiPageStats.stats}"
--      var="stat">["#{stat.date}", #{stat.count}],</h:list>[0,0]]}
--    </f:view>
--
package AWA.Counters.Beans is

   type Counter_Bean (Of_Type  : ADO.Objects.Object_Key_Type;
                      Of_Class : ADO.Schemas.Class_Mapping_Access) is
     new Util.Beans.Basic.Readonly_Bean with record
      Counter : Counter_Index_Type;
      Value   : Integer := -1;
      Object  : ADO.Objects.Object_Key (Of_Type, Of_Class);
   end record;
   type Counter_Bean_Access is access all Counter_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Counter_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   type Counter_Stat_Bean is new AWA.Counters.Models.Stat_List_Bean with record
      Module           : AWA.Counters.Modules.Counter_Module_Access;
      Stats            : aliased AWA.Counters.Models.Stat_Info_List_Bean;
      Stats_Bean       : AWA.Counters.Models.Stat_Info_List_Bean_Access;
   end record;
   type Counter_Stat_Bean_Access is access all Counter_Stat_Bean'Class;

   --  Get the query definition to collect the counter statistics.
   function Get_Query (From : in Counter_Stat_Bean) return ADO.Queries.Query_Definition_Access;

   overriding
   function Get_Value (List : in Counter_Stat_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Counter_Stat_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Load the statistics information.
   overriding
   procedure Load (List    : in out Counter_Stat_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Blog_Stat_Bean bean instance.
   function Create_Counter_Stat_Bean (Module : in Modules.Counter_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Counters.Beans;
