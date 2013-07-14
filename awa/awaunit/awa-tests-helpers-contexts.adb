-----------------------------------------------------------------------
--  users-tests-contexts -- Helpers for service contexts
--  Copyright (C) 2013 Stephane Carrez
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
