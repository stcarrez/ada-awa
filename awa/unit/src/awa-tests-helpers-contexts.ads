-----------------------------------------------------------------------
--  users-tests-contexts -- Helpers for service contexts
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Services.Contexts;
with Util.Beans.Objects;
with Util.Beans.Objects.Maps;
package AWA.Tests.Helpers.Contexts is

   type Service_Context is new AWA.Services.Contexts.Service_Context with record
      Session_Attributes : Util.Beans.Objects.Maps.Map;
   end record;

   --  Get the attribute registered under the given name in the HTTP session.
   overriding
   function Get_Session_Attribute (Ctx  : in Service_Context;
                                   Name : in String) return Util.Beans.Objects.Object;

   --  Set the attribute registered under the given name in the HTTP session.
   overriding
   procedure Set_Session_Attribute (Ctx   : in out Service_Context;
                                    Name  : in String;
                                    Value : in Util.Beans.Objects.Object);

end AWA.Tests.Helpers.Contexts;
