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

with Util.Properties.Bundles;

with ADO.Sessions;
with ADO.Queries;
with ADO.Statements;

with ASF.Models.Selects;

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

   --  Create the selector list from the SQL query.  The query will be executed.
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

end AWA.Helpers.Selectors;
