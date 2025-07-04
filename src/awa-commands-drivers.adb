-----------------------------------------------------------------------
--  awa-commands-drivers -- Driver for AWA commands for server or admin tool
--  Copyright (C) 2020, 2021, 2022, 2024, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Util.Strings.Formats;
with Servlet.Core;
package body AWA.Commands.Drivers is

   use Ada.Strings.Unbounded;
   use AWA.Applications;

   function "-" (Message : in String) return String is (Message);

   function Format (Message : in String;
                    Arg1    : in String) return String
     renames Util.Strings.Formats.Format;

   Help_Command            : aliased AWA.Commands.Drivers.Help_Command_Type;

   --  ------------------------------
   --  Setup the command before parsing the arguments and executing it.
   --  ------------------------------
   overriding
   procedure Setup (Command : in out Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration;
                    Context : in out Context_Type) is
   begin
      GC.Set_Usage (Config => Config,
                    Usage  => Command.Get_Name & " [arguments]",
                    Help   => Command.Get_Description);
      AWA.Commands.Setup_Command (Config, Context);
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

   --  ------------------------------
   --  Setup the command before parsing the arguments and executing it.
   --  ------------------------------
   overriding
   procedure Setup (Command : in out Application_Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration;
                    Context : in out Context_Type) is
   begin
      GC.Define_Switch (Config => Config,
                        Output => Command.Application_Name'Access,
                        Switch => "-a:",
                        Long_Switch => "--application=",
                        Argument => "NAME",
                        Help   => -("Defines the name or URI of the application"));
      AWA.Commands.Setup_Command (Config, Context);
   end Setup;

   function Is_Application (Command : in Application_Command_Type;
                            URI     : in String) return Boolean is
   begin
      return Command.Application_Name.all = URI
        or else (URI (URI'First) = '/'
                   and then Command.Application_Name.all = URI (URI'First + 1 .. URI'Last));
   end Is_Application;

   overriding
   procedure Execute (Command   : in out Application_Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Name);

      procedure Find (URI : in String;
                      App : in Servlet.Core.Servlet_Registry_Access);

      Count    : Natural := 0;
      Selected : Application_Access;
      App_Name : Unbounded_String;

      procedure Find (URI : in String;
                      App : in Servlet.Core.Servlet_Registry_Access) is
      begin
         if App.all in Application'Class then
            if Command.Is_Application (URI) then
               App_Name := To_Unbounded_String (URI (URI'First + 1 .. URI'Last));
               Selected := Application'Class (App.all)'Unchecked_Access;
               Count := Count + 1;
            elsif Command.Application_Name'Length = 0 then
               App_Name := To_Unbounded_String (URI (URI'First + 1 .. URI'Last));
               Selected := Application'Class (App.all)'Unchecked_Access;
               Count := Count + 1;
            end if;
         end if;
      end Find;

   begin
      WS.Iterate (Find'Access);
      if Count /= 1 then
         Context.Console.Notice (N_ERROR, -("No application found"));
         return;
      end if;

      Configure (To_String (App_Name), Context);
      if Command.Initialize_Application then
         Selected.Initialize (Context.App_Config, Context.Factory);
      end if;
      Application_Command_Type'Class (Command).Execute (Selected.all, Args, Context);
   end Execute;

   --  ------------------------------
   --  Print the command usage.
   --  ------------------------------
   procedure Usage (Args    : in Argument_List'Class;
                    Context : in out Context_Type;
                    Name    : in String := "") is
   begin
      GC.Display_Help (Context.Command_Config);
      if Name'Length > 0 then
         Driver.Usage (Args, Context, Name);
      end if;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Usage;

   --  ------------------------------
   --  Execute the command with its arguments.
   --  ------------------------------
   procedure Execute (Name    : in String;
                      Args    : in Argument_List'Class;
                      Context : in out Context_Type) is
   begin
      Driver.Execute (Name, Args, Context);
   end Execute;

   --  ------------------------------
   --  Get the server configuration file path.
   --  ------------------------------
   function Get_Configuration_Path (Context : in out Context_Type) return String is
   begin
      if Context.Config_File'Length = 0 then
         return Driver_Name & ".properties";
      else
         return Context.Config_File.all;
      end if;
   end Get_Configuration_Path;

   procedure Run (Context   : in out Context_Type;
                  Arguments : out Util.Commands.Dynamic_Argument_List) is
   begin
      GC.Set_Usage (Config => Context.Command_Config,
                    Usage  => "[switchs] command [arguments]",
                    Help   => Format (-("{0} - server commands"), Driver_Name));
      GC.Getopt (Config => Context.Command_Config);
      Util.Commands.Parsers.GNAT_Parser.Get_Arguments (Arguments, GC.Get_Argument);

      if Context.Debug or else Context.Verbose or else Context.Dump then
         Configure_Logs (Root    => Context.Global_Config.Get ("log4j.rootCategory", ""),
                         Debug   => Context.Debug,
                         Dump    => Context.Dump,
                         Verbose => Context.Verbose);
      end if;

      Context.Load_Configuration (Get_Configuration_Path (Context));

      declare
         Cmd_Name : constant String := Arguments.Get_Command_Name;
      begin
         if Cmd_Name'Length = 0 then
            Context.Console.Notice (N_ERROR, -("Missing command name to execute."));
            Usage (Arguments, Context);
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
         end if;
         Execute (Cmd_Name, Arguments, Context);

      exception
         when GNAT.Command_Line.Invalid_Parameter =>
            Context.Console.Notice (N_ERROR, -("Missing option parameter"));
            raise Error;
      end;

   end Run;

begin
   Driver.Add_Command ("help",
                       -("print some help"),
                       Help_Command'Access);
end AWA.Commands.Drivers;
