-----------------------------------------------------------------------
--  awa-users-module -- User management module
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Applications;

with AWA.Modules;
with AWA.Users.Services;
with AWA.Users.Filters;
with AWA.Users.Servlets;

--  == Integration ==
--  The `User_Module` manages the creation, update, removal of users
--  in an application.  It provides operations that are used by the user
--  beans or other services to create and update wiki pages.
--  An instance of the `User_Module` must be declared and registered in the
--  AWA application.  The module instance can be defined as follows:
--
--    type Application is new AWA.Applications.Application with record
--       User_Module : aliased AWA.Users.Modules.User_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Users.Modules.NAME,
--              Module => App.User_Module'Access);
--
package AWA.Users.Modules is

   NAME : constant String := "users";

   type User_Module is new AWA.Modules.Module with private;
   type User_Module_Access is access all User_Module'Class;

   --  Initialize the user module.
   overriding
   procedure Initialize (Plugin : in out User_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Configures the module after its initialization and after having read
   --  its XML configuration.
   overriding
   procedure Configure (Plugin : in out User_Module;
                        Props  : in ASF.Applications.Config);

   --  Get the user manager.
   function Get_User_Manager (Plugin : in User_Module) return Services.User_Service_Access;

   --  Create a user manager.  This operation can be overridden to provide another
   --  user service implementation.
   function Create_User_Manager (Plugin : in User_Module) return Services.User_Service_Access;

   --  Get the user module instance associated with the current application.
   function Get_User_Module return User_Module_Access;

   --  Get the user manager instance associated with the current application.
   function Get_User_Manager return Services.User_Service_Access;

private

   type User_Module is new AWA.Modules.Module with record
      Manager     : Services.User_Service_Access := null;
      Auth_Filter : aliased AWA.Users.Filters.Auth_Filter;
      Auth        : aliased AWA.Users.Servlets.Request_Auth_Servlet;
      Verify_Auth : aliased AWA.Users.Servlets.Verify_Auth_Servlet;
      Verify_Key  : aliased AWA.Users.Servlets.Verify_Key_Servlet;
   end record;

end AWA.Users.Modules;
