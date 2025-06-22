-----------------------------------------------------------------------
--  awa-applications-factory -- Factory for AWA Applications
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Applications.Main;
with Security.Policies;
package AWA.Applications.Factory is

   --  ------------------------------
   --  Application factory to setup the permission manager.
   --  ------------------------------
   type Application_Factory is new ASF.Applications.Main.Application_Factory with private;

   --  Create the security manager.  The security manager is created during
   --  the initialization phase of the application.  This implementation
   --  creates a <b>AWA.Permissions.Services.Permission_Manager</b> object.
   overriding
   function Create_Security_Manager (App : in Application_Factory)
                                     return Security.Policies.Policy_Manager_Access;

   --  Set the application instance that will be used when creating the permission manager.
   procedure Set_Application (Factory : in out ASF.Applications.Main.Application_Factory'Class;
                              App     : in Application_Access);
private

   type Application_Factory is new ASF.Applications.Main.Application_Factory with record
      App : Application_Access := null;
   end record;

end AWA.Applications.Factory;
