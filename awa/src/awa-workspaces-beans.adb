-----------------------------------------------------------------------
--  awa-workspaces-beans -- Beans for module workspaces
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

with ASF.Events.Actions;
package body Awa.Workspaces.Beans is

   --  ------------------------------
   --  Example of action method.
   --  ------------------------------
   procedure Action (Bean    : in out Workspaces_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Action;

   package Action_Binding is
     new ASF.Events.Actions.Action_Method.Bind (Bean   => Workspaces_Bean,
                                                Method => Action,
                                                Name   => "action");

   Workspaces_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Action_Binding.Proxy'Access, null);

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Workspaces_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (From.Count);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Workspaces_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "count" then
         From.Count := Util.Beans.Objects.To_Integer (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Workspaces_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Workspaces_Bean_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Create the Workspaces_Bean bean instance.
   --  ------------------------------
   function Create_Workspaces_Bean (Module : in Awa.Workspaces.Module.Workspaces_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Workspaces_Bean_Access := new Workspaces_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Workspaces_Bean;

end Awa.Workspaces.Beans;
