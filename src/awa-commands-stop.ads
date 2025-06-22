-----------------------------------------------------------------------
--  awa-commands-stop -- Command to stop the web server
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with GNAT.Command_Line;
with AWA.Commands.Drivers;
generic
   with package Command_Drivers is new AWA.Commands.Drivers (<>);
package AWA.Commands.Stop is

   type Command_Type is new Command_Drivers.Command_Type with record
      Management_Port   : aliased Integer := 0;
   end record;

   --  Setup the command before parsing the arguments and executing it.
   overriding
   procedure Setup (Command : in out Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration;
                    Context : in out Context_Type);

   --  Stop the server by sending a 'stop' command on the management socket.
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type);

   Command       : aliased Command_Type;

end AWA.Commands.Stop;
