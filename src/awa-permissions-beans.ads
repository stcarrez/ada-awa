-----------------------------------------------------------------------
--  awa-permissions-beans -- Permission beans
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Methods;

with AWA.Events;
with AWA.Permissions.Models;

package AWA.Permissions.Beans is

   type Permission_Bean is new AWA.Permissions.Models.ACL_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with record
      Permission : Ada.Strings.Unbounded.Unbounded_String;
      Kind       : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Permission_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Permission_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Create a new permission.
   procedure Create (Bean    : in out Permission_Bean;
                     Event   : in AWA.Events.Module_Event'Class);

   --  Create the permission bean instance.
   function Create_Permission_Bean return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Permissions.Beans;
