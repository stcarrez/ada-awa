-----------------------------------------------------------------------
--  akt-commands-info -- Info command to describe the current configuration
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with GNAT.Command_Line;
with AWA.Commands.Drivers;
with AWA.Applications;
with AWA.Modules;
with ASF.Locales;
generic
   with package Command_Drivers is new AWA.Commands.Drivers (<>);
package AWA.Commands.Info is

   type Command_Type is new Command_Drivers.Application_Command_Type with record
      Bundle        : ASF.Locales.Bundle;
      Long_List     : aliased Boolean := False;
      Value_Length  : Positive := 50;
   end record;

   --  Print the configuration about the application.
   overriding
   procedure Execute (Command     : in out Command_Type;
                      Application : in out AWA.Applications.Application'Class;
                      Args        : in Argument_List'Class;
                      Context     : in out Context_Type);

   --  Setup the command before parsing the arguments and executing it.
   overriding
   procedure Setup (Command : in out Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration;
                    Context : in out Context_Type);

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in out Command_Type;
                   Name      : in String;
                   Context   : in out Context_Type);

   --  Print the configuration identified by the given name.
   procedure Print (Command     : in out Command_Type;
                    Name        : in String;
                    Value       : in String;
                    Default     : in String;
                    Context     : in out Context_Type);
   procedure Print (Command     : in out Command_Type;
                    Application : in out AWA.Applications.Application'Class;
                    Name        : in String;
                    Default     : in String;
                    Context     : in out Context_Type);
   procedure Print (Command     : in out Command_Type;
                    Module      : in AWA.Modules.Module'Class;
                    Name        : in String;
                    Default     : in String;
                    Context     : in out Context_Type);

   Command       : aliased Command_Type;

end AWA.Commands.Info;
