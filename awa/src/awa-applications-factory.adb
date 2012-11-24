-----------------------------------------------------------------------
--  awa-applications-factory -- Factory for AWA Applications
--  Copyright (C) 2011, 2012 Stephane Carrez
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
