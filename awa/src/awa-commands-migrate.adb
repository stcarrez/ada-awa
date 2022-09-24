-----------------------------------------------------------------------
--  awa-commands-migrate -- Database schema migration command
--  Copyright (C) 2022 Stephane Carrez
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
with Util.Files;
with Util.Strings.Vectors;
with ASF.Applications.Main;
with ADO.Configs;
with ADO.Sessions;
with ADO.Sessions.Sources;
with ADO.Sessions.Factory;
with ADO.Schemas.Databases;
package body AWA.Commands.Migrate is

   --  ------------------------------
   --  Check and run the SQL migration scripts if necessary.
   --  ------------------------------
   overriding
   procedure Execute (Command     : in out Command_Type;
                      Application : in out AWA.Applications.Application'Class;
                      Args        : in Argument_List'Class;
                      Context     : in out Context_Type) is
      pragma Unreferenced (Args);

      procedure Scan_Migration (Path : in String;
                                Done : out Boolean);

      Connection : ADO.Sessions.Sources.Data_Source;
      Factory : ADO.Sessions.Factory.Session_Factory;
      Session : ADO.Sessions.Master_Session;
      List    : ADO.Schemas.Databases.Upgrade_List;
      Files   : Util.Strings.Vectors.Vector;

      procedure Scan_Migration (Path : in String;
                                Done : out Boolean) is
      begin
         Done := False;
         ADO.Schemas.Databases.Scan_Migration (Session, Path, List);
      end Scan_Migration;

   begin
      --  Do a manual initialization of some components but not all of them
      --  because we don't want to have AWA or a module that accesses the database
      --  (the current database schema may be wrong for the application).
      ASF.Applications.Main.Application (Application).Initialize_Components;
      ASF.Applications.Main.Application (Application).Initialize_Config (Context.App_Config);
      Context.App_Config.Set (ADO.Configs.NO_ENTITY_LOAD, "true");

      --  Configure the database connection by usingg the computed application config.
      ADO.Configs.Initialize (Context.App_Config);
      Connection.Set_Connection (Context.App_Config.Get (AWA.Applications.P_Database.P));
      Connection.Set_Property (ADO.Configs.QUERY_PATHS_CONFIG,
                               Context.App_Config.Get (ADO.Configs.QUERY_PATHS_CONFIG,
                                                       "db"));
      Connection.Set_Property (ADO.Configs.MIGRATE_PATHS_CONFIG,
                               Context.App_Config.Get (ADO.Configs.MIGRATE_PATHS_CONFIG,
                                                       "db/migrate"));
      Connection.Set_Property (ADO.Configs.QUERY_LOAD_CONFIG, "false");
      Connection.Set_Property (ADO.Configs.NO_ENTITY_LOAD, "true");
      Factory.Create (Connection);

      --  Get a writable database session for the migration.
      Session := Factory.Get_Master_Session;

      Util.Files.Iterate_Path (Connection.Get_Property (Ado.Configs.Migrate_Paths_config),
                               Scan_Migration'Access);
      if List.Is_Empty then
         Context.Console.Notice (N_INFO, "Database schema is up to date");
         return;
      end if;

      ADO.Schemas.Databases.Sort_Migration (List);
      if Command.Execute then
         for Upgrade of List loop
            ADO.Schemas.Databases.Run_Migration
              (Session, Upgrade, Files, ADO.Sessions.Execute'Access);
         end loop;
         Context.Console.Notice (N_INFO, "Database schema was migrated");
      else
         Context.Console.Notice (N_INFO, "Database schema migration is necessary"
                                   & " (launch again with --execute option)");
         Context.Console.Notice (N_Info, "The following SQL scripts must be executed:");
         for Upgrade of List loop
            ADO.Schemas.Databases.Prepare_Migration
              (Session, Upgrade, Files);
            for Path of Files loop
               Context.Console.Notice (N_INFO, Path);
            end loop;
         end loop;
      end if;
   end Execute;

   --  ------------------------------
   --  Setup the command before parsing the arguments and executing it.
   --  ------------------------------
   overriding
   procedure Setup (Command : in out Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration;
                    Context : in out Context_Type) is
   begin
      Command.Initialize_Application := False;
      GC.Set_Usage (Config => Config,
                    Usage  => Command.Get_Name & " [arguments]",
                    Help   => Command.Get_Description);
      Command_Drivers.Application_Command_Type (Command).Setup (Config, Context);
      GC.Define_Switch (Config => Config,
                        Output => Command.Execute'Access,
                        Long_Switch => "--execute",
                        Help   => -("Execute the SQL to upgrade the database"));
   end Setup;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in out Command_Type;
                   Name      : in String;
                   Context   : in out Context_Type) is
      pragma Unreferenced (Command, Context);
   begin
      null;
   end Help;

begin
   Command_Drivers.Driver.Add_Command ("migrate",
                                       -("database schema upgrade"),
                                       Command'Access);
end AWA.Commands.Migrate;
