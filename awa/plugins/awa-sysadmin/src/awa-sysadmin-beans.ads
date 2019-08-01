-----------------------------------------------------------------------
--  awa-sysadmin-beans -- Sysadmin specific Ada beans
--  Copyright (C) 2019 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Util.Beans.Basic;

with AWA.Sysadmin.Modules;
with AWA.Sysadmin.Models;
package AWA.Sysadmin.Beans is

   use Ada.Strings.Unbounded;

   type Authenticate_Bean is new AWA.Sysadmin.Models.Authenticate_Bean with null record;
   type Authenticate_Bean_Access is access all Authenticate_Bean'Class;

   --  Action to authenticate the sysadmin user.
   overriding
   procedure Authenticate (Data    : in out Authenticate_Bean;
                           Outcome : in out Unbounded_String);

   --  Create an authenticate bean.
   function Create_Authenticate_Bean (Module : in AWA.Sysadmin.Modules.Sysadmin_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Sysadmin.Beans;
