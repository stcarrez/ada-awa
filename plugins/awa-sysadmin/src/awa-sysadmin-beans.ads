-----------------------------------------------------------------------
--  awa-sysadmin-beans -- Sysadmin specific Ada beans
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
