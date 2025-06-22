-----------------------------------------------------------------------
--  awa-changelogs-modules -- Module changelogs
--  Copyright (C) 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Applications;

with ADO;

with AWA.Modules;
package AWA.Changelogs.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "changelogs";

   --  ------------------------------
   --  Module changelogs
   --  ------------------------------
   type Changelog_Module is new AWA.Modules.Module with private;
   type Changelog_Module_Access is access all Changelog_Module'Class;

   --  Initialize the changelogs module.
   overriding
   procedure Initialize (Plugin : in out Changelog_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the changelogs module.
   function Get_Changelog_Module return Changelog_Module_Access;

   --  Add the log message and associate it with the database entity identified by
   --  the given id and the entity type.  The log message is associated with the current user.
   procedure Add_Log (Model       : in Changelog_Module;
                      Id          : in ADO.Identifier;
                      Entity_Type : in String;
                      Message     : in String);

private

   type Changelog_Module is new AWA.Modules.Module with null record;

end AWA.Changelogs.Modules;
