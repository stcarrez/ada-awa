-----------------------------------------------------------------------
--  AWA.Sysadmin.Models -- AWA.Sysadmin.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-body.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.2.3
-----------------------------------------------------------------------
--  Copyright (C) 2021 Stephane Carrez
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
pragma Warnings (Off);
with Ada.Unchecked_Deallocation;
with ASF.Events.Faces.Actions;
pragma Warnings (On);
package body AWA.Sysadmin.Models is

   pragma Style_Checks ("-mr");
   pragma Warnings (Off, "formal parameter * is not referenced");
   pragma Warnings (Off, "use clause for type *");
   pragma Warnings (Off, "use clause for private type *");

   use type ADO.Objects.Object_Record_Access;
   use type ADO.Objects.Object_Ref;


   procedure Op_Authenticate (Bean    : in out Authenticate_Bean;
                              Outcome : in out Ada.Strings.Unbounded.Unbounded_String);
   procedure Op_Authenticate (Bean    : in out Authenticate_Bean;
                              Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Authenticate_Bean'Class (Bean).Authenticate (Outcome);
   end Op_Authenticate;
   package Binding_Authenticate_Bean_1 is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Authenticate_Bean,
                                                      Method => Op_Authenticate,
                                                      Name   => "authenticate");

   Binding_Authenticate_Bean_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Binding_Authenticate_Bean_1.Proxy'Access
     );

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression.
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Authenticate_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Binding_Authenticate_Bean_Array'Access;
   end Get_Method_Bindings;
   --  ------------------------------
   --  Get the bean attribute identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Authenticate_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "password" then
         return Util.Beans.Objects.To_Object (From.Password);
      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;


   --  ------------------------------
   --  Set the value identified by the name
   --  ------------------------------
   overriding
   procedure Set_Value (Item  : in out Authenticate_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "password" then
         Item.Password := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;


end AWA.Sysadmin.Models;
