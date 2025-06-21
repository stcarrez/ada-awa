-----------------------------------------------------------------------
--  awa-permissions-beans -- Permission beans
--  Copyright (C) 2015, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Security.Permissions;

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
      elsif Name = "workspace_id" then
         From.Set_Workspace_Id (ADO.Utils.To_Identifier (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Permission_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Permission_Bean_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Create a new permission.
   --  ------------------------------
   procedure Create (Bean    : in out Permission_Bean;
                     Event   : in AWA.Events.Module_Event'Class) is
      pragma Unreferenced (Event);

      Ctx      : constant ASC.Service_Context_Access := ASC.Current;
      Manager  : constant Services.Permission_Manager_Access
        := Services.Get_Permission_Manager (Ctx);
      List     : constant Security.Permissions.Permission_Index_Array
        := Security.Permissions.Get_Permission_Array (To_String (Bean.Permission));
      DB       : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Kind     : ADO.Entity_Type;
   begin
      Ctx.Start;
      Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, To_String (Bean.Kind));
      for Perm of List loop
         Manager.Add_Permission (Session    => DB,
                                 User       => Ctx.Get_User_Identifier,
                                 Entity     => Bean.Get_Entity_Id,
                                 Kind       => Kind,
                                 Workspace  => Bean.Get_Workspace_Id,
                                 Permission => Perm);
      end loop;
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
