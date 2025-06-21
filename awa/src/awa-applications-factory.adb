-----------------------------------------------------------------------
--  awa-applications-factory -- Factory for AWA Applications
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Permissions.Services;
package body AWA.Applications.Factory is

   --  ------------------------------
   --  Create the security manager.  The security manager is created during
   --  the initialization phase of the application.  This implementation
   --  creates a <b>AWA.Permissions.Services.Permission_Manager</b> object.
   --  ------------------------------
   overriding
   function Create_Security_Manager (App : in Application_Factory)
                                       return Security.Policies.Policy_Manager_Access is
   begin
      return AWA.Permissions.Services.Create_Permission_Manager (App.App);
   end Create_Security_Manager;

   --  ------------------------------
   --  Set the application instance that will be used when creating the permission manager.
   --  ------------------------------
   procedure Set_Application (Factory : in out ASF.Applications.Main.Application_Factory'Class;
                              App     : in Application_Access) is
   begin
      if Factory in Application_Factory'Class then
         Application_Factory'Class (Factory).App := App;
      end if;
   end Set_Application;

end AWA.Applications.Factory;
