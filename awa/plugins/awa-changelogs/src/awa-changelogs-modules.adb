-----------------------------------------------------------------------
--  awa-changelogs-modules -- Module changelogs
--  Copyright (C) 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Calendar;

with Util.Log.Loggers;

with AWA.Modules.Get;
with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Changelogs.Models;

with ADO.Sessions;
with ADO.Sessions.Entities;
package body AWA.Changelogs.Modules is

   package ASC renames AWA.Services.Contexts;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Awa.Changelogs.Module");

   --  ------------------------------
   --  Initialize the changelogs module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Changelog_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the changelogs module");

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the changelogs module.
   --  ------------------------------
   function Get_Changelog_Module return Changelog_Module_Access is
      function Get is new AWA.Modules.Get (Changelog_Module, Changelog_Module_Access, NAME);
   begin
      return Get;
   end Get_Changelog_Module;

   --  ------------------------------
   --  Add the log message and associate it with the database entity identified by
   --  the given id and the entity type.  The log message is associated with the current user.
   --  ------------------------------
   procedure Add_Log (Model       : in Changelog_Module;
                      Id          : in ADO.Identifier;
                      Entity_Type : in String;
                      Message     : in String) is
      pragma Unreferenced (Model);

      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      User    : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      DB      : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Kind    : ADO.Entity_Type;
      History : AWA.Changelogs.Models.Changelog_Ref;
   begin
      Ctx.Start;
      Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, Entity_Type);
      History.Set_For_Entity_Id (Id);
      History.Set_User (User);
      History.Set_Date (Ada.Calendar.Clock);
      History.Set_Entity_Type (Kind);
      History.Set_Text (Message);
      History.Save (DB);
      DB.Commit;
   end Add_Log;

end AWA.Changelogs.Modules;
