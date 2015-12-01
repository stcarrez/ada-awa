-----------------------------------------------------------------------
--  awa-counters-beans -- Counter bean definition
--  Copyright (C) 2015 Stephane Carrez
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
with ADO.Objects;
with ADO.Schemas;
with Util.Beans.Objects;
with Util.Beans.Basic;

--  == Counter Bean ==
--  The <b>Counter_Bean</b> allows to represent a counter associated with some database
--  entity and allows its control by the <awa:counter> component.
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

end AWA.Counters.Beans;
