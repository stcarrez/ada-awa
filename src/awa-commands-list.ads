-----------------------------------------------------------------------
--  akt-commands-list -- List commands to report database information for admin tool
--  Copyright (C) 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with GNAT.Command_Line;
with AWA.Commands.Drivers;
with AWA.Applications;
with ASF.Locales;
generic
   with package Command_Drivers is new AWA.Commands.Drivers (<>);
package AWA.Commands.List is

   type Command_Type is new Command_Drivers.Application_Command_Type with record
      Bundle        : ASF.Locales.Bundle;
      List_Users    : aliased Boolean := False;
      List_Jobs     : aliased Boolean := False;
      List_Tables   : aliased Boolean := False;
      List_Sessions : aliased Boolean := False;
      List_Audits   : aliased Boolean := False;
   end record;

   --  List some database information.
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

end AWA.Commands.List;
