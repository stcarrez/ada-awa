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
