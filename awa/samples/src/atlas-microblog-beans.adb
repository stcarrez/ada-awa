-----------------------------------------------------------------------
--  atlas-microblog-beans -- Beans for module microblog
--  Copyright (C) 2012 Stephane Carrez
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

with ASF.Events.Faces.Actions;
package body Atlas.Microblog.Beans is

   --  ------------------------------
   --  Example of action method.
   --  ------------------------------
   procedure Post (Bean    : in out Microblog_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Create (Bean.Post);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");
   end Post;

   package Action_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Microblog_Bean,
                                                      Method => Post,
                                                      Name   => "post");

   Microblog_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Action_Binding.Proxy'Access, null);

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Microblog_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Post.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return From.Post.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Microblog_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "message" then
         From.Post.Set_Message (Util.Beans.Objects.To_String (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Microblog_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Microblog_Bean_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Create the Microblog_Bean bean instance.
   --  ------------------------------
   function Create_Microblog_Bean (Module : in Atlas.Microblog.Modules.Microblog_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Microblog_Bean_Access := new Microblog_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Microblog_Bean;

end Atlas.Microblog.Beans;
