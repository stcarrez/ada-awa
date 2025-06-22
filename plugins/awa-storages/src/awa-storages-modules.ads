-----------------------------------------------------------------------
--  awa-storages-modules -- Storage management module
--  Copyright (C) 2012, 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Applications;

with AWA.Modules;
with AWA.Storages.Services;
with AWA.Storages.Servlets;

--  == Integration ==
--  To be able to use the `storages` module, you will need to add the following
--  line in your GNAT project file:
--
--    with "awa_storages";
--
--  The `Storage_Module` type represents the storage module.  An instance
--  of the storage module must be declared and registered when the application
--  is created and initialized.  The storage module is associated with
--  the storage service which provides and implements the storage management
--  operations.  An instance of the `Storage_Module` must be declared and
--  registered in the AWA application.  The module instance can be defined
--  as follows:
--
--    with AWA.Storages.Modules;
--    ...
--    type Application is new AWA.Applications.Application with record
--       Storage_Module : aliased AWA.Storages.Modules.Storage_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Storages.Modules.NAME,
--              Module => App.Storage_Module'Access);
--
--  == Permissions ==
--  @include-permission storages.xml
--
--  == Configuration ==
--  The `storages` module defines the following configuration parameters:
--
--  @include-config storages.xml
package AWA.Storages.Modules is

   NAME : constant String := "storages";

   type Storage_Module is new AWA.Modules.Module with private;
   type Storage_Module_Access is access all Storage_Module'Class;

   --  Initialize the storage module.
   overriding
   procedure Initialize (Plugin : in out Storage_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Configures the module after its initialization and after having
   --  read its XML configuration.
   overriding
   procedure Configure (Plugin : in out Storage_Module;
                        Props  : in ASF.Applications.Config);

   --  Get the storage manager.
   function Get_Storage_Manager (Plugin : in Storage_Module)
                                 return Services.Storage_Service_Access;

   --  Create a storage manager.  This operation can be overridden to provide
   --  another storage service implementation.
   function Create_Storage_Manager (Plugin : in Storage_Module)
                                    return Services.Storage_Service_Access;

   --  Get the storage module instance associated with the current application.
   function Get_Storage_Module return Storage_Module_Access;

   --  Get the storage manager instance associated with the current application.
   function Get_Storage_Manager return Services.Storage_Service_Access;

private

   type Storage_Module is new AWA.Modules.Module with record
      Manager         : Services.Storage_Service_Access := null;
      Storage_Servlet : aliased AWA.Storages.Servlets.Storage_Servlet;
   end record;

end AWA.Storages.Modules;
