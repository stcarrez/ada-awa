-----------------------------------------------------------------------
--  awa-commands-setup -- Setup command to start and configure the application
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with GNAT.Command_Line;
with AWA.Commands.Start;
generic
   with package Start_Command is new AWA.Commands.Start (<>);
package AWA.Commands.Setup is

   type Command_Type is new Start_Command.Command_Type with record
      List_Sessions : aliased Boolean := False;
   end record;

   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type);

   --  Setup the command before parsing the arguments and executing it.
   overriding
   procedure Setup (Command : in out Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration;
                    Context : in out Context_Type);

   Command       : aliased Command_Type;

end AWA.Commands.Setup;
