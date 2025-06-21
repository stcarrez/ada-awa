-----------------------------------------------------------------------
--  awa-helpers -- Helpers for AWA applications
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Properties.Bundles;

with ADO.Sessions;
with ADO.Queries;
with ADO.Statements;

with ASF.Models.Selects;
with ASF.Contexts.Faces;

--  The <b>Selectors</b> package provides several helper operations to create and populate
--  a selector list from an static list (type) or from by using a database SQL query.
package AWA.Helpers.Selectors is

   --  Create a selector list from the definition of a discrete type such as an enum.
   --  The select item has the enum value as value and the label is created by
   --  localizing the string <b>Prefix</b>_<i>enum name</i>.
   generic
      type T is (<>);
      Prefix : String;
   function Create_From_Enum (Bundle : in Util.Properties.Bundles.Manager'Class)
                              return ASF.Models.Selects.Select_Item_List;

   --  Create a selector list by using a resource bundle and a create operation that looks for
   --  messages in the bundle.  The bundle name <b>Bundle</b> gives the name of the resource
   --  bundled to load.  The locale is determined by the ASF context passed in <b>Context</b>.
   --  The <b>Create</b> function is in charge of creating and populating the select list.
   function Create_Selector_Bean (Bundle  : in String;
                                  Context : in ASF.Contexts.Faces.Faces_Context_Access := null;
                                  Create  : access function
                                    (Bundle : in Util.Properties.Bundles.Manager'Class)
                                  return ASF.Models.Selects.Select_Item_List)
                                  return Util.Beans.Basic.Readonly_Bean_Access;

   --  Append the selector list from the SQL query.  The query will be executed.
   --  It should return rows with at least two columns.  The first column is the
   --  selector value and the second column is the selector label.
   procedure Append_From_Query (Into  : in out ASF.Models.Selects.Select_Item_List;
                                Query : in out ADO.Statements.Query_Statement'Class);

   --  Create the selector list from the SQL query.  The query will be executed.
   --  It should return rows with at least two columns.  The first column is the
   --  selector value and the second column is the selector label.
   function Create_From_Query (Session : in ADO.Sessions.Session'Class;
                               Query   : in ADO.Queries.Context'Class)
                               return ASF.Models.Selects.Select_Item_List;

   --  ------------------------------
   --  Select list bean
   --  ------------------------------
   --  The <b>Select_List_Bean</b> type is a bean object that can be declared in XML
   --  configuration files and customized by the <b>query</b> property.
   type Select_List_Bean is new Util.Beans.Basic.Bean with record
      List : ASF.Models.Selects.Select_Item_List;
   end record;
   type Select_List_Bean_Access is access all Select_List_Bean;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Select_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   overriding
   procedure Set_Value (From  : in out Select_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create the select list bean instance.
   function Create_Select_List_Bean return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Helpers.Selectors;
