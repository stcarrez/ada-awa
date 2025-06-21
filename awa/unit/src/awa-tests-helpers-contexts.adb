-----------------------------------------------------------------------
--  users-tests-contexts -- Helpers for service contexts
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body AWA.Tests.Helpers.Contexts is

   --  ------------------------------
   --  Get the attribute registered under the given name in the HTTP session.
   --  ------------------------------
   overriding
   function Get_Session_Attribute (Ctx  : in Service_Context;
                                   Name : in String) return Util.Beans.Objects.Object is
      Pos : constant Util.Beans.Objects.Maps.Cursor := Ctx.Session_Attributes.Find (Name);
   begin
      if Util.Beans.Objects.Maps.Has_Element (Pos) then
         return Util.Beans.Objects.Maps.Element (Pos);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Session_Attribute;

   --  ------------------------------
   --  Set the attribute registered under the given name in the HTTP session.
   --  ------------------------------
   overriding
   procedure Set_Session_Attribute (Ctx   : in out Service_Context;
                                    Name  : in String;
                                    Value : in Util.Beans.Objects.Object) is
   begin
      Ctx.Session_Attributes.Include (Name, Value);
   end Set_Session_Attribute;

end AWA.Tests.Helpers.Contexts;
