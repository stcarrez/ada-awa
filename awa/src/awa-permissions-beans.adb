-----------------------------------------------------------------------
--  awa-permissions-beans -- Permission beans
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

with ADO.Utils;
with ADO.Sessions;
with ADO.Sessions.Entities;

with AWA.Services.Contexts;
with AWA.Events.Action_Method;
with AWA.Permissions.Services;

package body AWA.Permissions.Beans is

   use Ada.Strings.Unbounded;

   package ASC renames AWA.Services.Contexts;

   package Create_Binding is
     new AWA.Events.Action_Method.Bind (Bean   => Permission_Bean,
                                        Method => Create,
                                        Name   => "create");

   Permission_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Create_Binding.Proxy'Access);

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Permission_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "entity_id" then
         From.Set_Entity_Id (ADO.Utils.To_Identifier (Value));
      elsif Name = "permission" then
         From.Permission := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "entity_type" then
         From.Kind := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Permission_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
   begin
      return Permission_Bean_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Create a new permission.
   --  ------------------------------
   procedure Create (Bean    : in out Permission_Bean;
                     Event   : in AWA.Events.Module_Event'Class) is
      Ctx      : constant ASC.Service_Context_Access := ASC.Current;
      DB       : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Kind     : ADO.Entity_Type;
   begin
      Ctx.Start;
      Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, To_String (Bean.Kind));
      AWA.Permissions.Services.Add_Permission (Session    => DB,
                                               Entity     => Bean.Get_Entity_Id,
                                               Kind       => Kind,
                                               User       => Ctx.Get_User_Identifier);
      Ctx.Commit;
   end Create;

   --  ------------------------------
   --  Create the permission bean instance.
   --  ------------------------------
   function Create_Permission_Bean return Util.Beans.Basic.Readonly_Bean_Access is
   begin
      return new Permission_Bean;
   end Create_Permission_Bean;

end AWA.Permissions.Beans;
