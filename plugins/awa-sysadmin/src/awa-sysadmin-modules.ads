-----------------------------------------------------------------------
--  awa-sysadmin-modules -- Module sysadmin
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Applications;

with AWA.Modules;
private with Servlet.Core.Rest;
private with AWA.Sysadmin.Filters;
package AWA.Sysadmin.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "sysadmin";

   --  ------------------------------
   --  Module sysadmin
   --  ------------------------------
   type Sysadmin_Module is new AWA.Modules.Module with private;
   type Sysadmin_Module_Access is access all Sysadmin_Module'Class;

   --  Initialize the sysadmin module.
   overriding
   procedure Initialize (Plugin : in out Sysadmin_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the sysadmin module.
   function Get_Sysadmin_Module return Sysadmin_Module_Access;

private

   type Sysadmin_Module is new AWA.Modules.Module with record
      API_Servlet : aliased Servlet.Core.Rest.Rest_Servlet;
      API_Filter  : aliased AWA.Sysadmin.Filters.Auth_Filter;
   end record;

end AWA.Sysadmin.Modules;
