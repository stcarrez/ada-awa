-----------------------------------------------------------------------
--  awa-commands-migrate -- Database schema migration command
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with GNAT.Command_Line;
with AWA.Commands.Drivers;
with AWA.Applications;
with ASF.Locales;
generic
   with package Command_Drivers is new AWA.Commands.Drivers (<>);
package AWA.Commands.Migrate is

   type Command_Type is new Command_Drivers.Application_Command_Type with record
      Bundle        : ASF.Locales.Bundle;
      Execute       : aliased Boolean := False;
   end record;

   --  Check and run the SQL migration scripts if necessary.
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

   Command       : aliased Command_Type;

end AWA.Commands.Migrate;
