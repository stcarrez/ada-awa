-----------------------------------------------------------------------
--  awa-commands-drivers -- Driver for AWA commands for server or admin tool
--  Copyright (C) 2020, 2021 Stephane Carrez
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
with GNAT.Command_Line;
with GNAT.Strings;
with Util.Commands.Drivers;
with Util.Commands.Parsers.GNAT_Parser;
with Servlet.Server;
with AWA.Applications;
generic
   Driver_Name : String;
   type Container_Type is limited new Servlet.Server.Container with private;
package AWA.Commands.Drivers is

   package Main_Driver is
     new Util.Commands.Drivers (Context_Type  => Context_Type,
                                Config_Parser => Util.Commands.Parsers.GNAT_Parser.Config_Parser,
                                Driver_Name   => Driver_Name);

   subtype Help_Command_Type is Main_Driver.Help_Command_Type;
   subtype Driver_Type is Main_Driver.Driver_Type;

   --  Get the server configuration file path.
   function Get_Configuration_Path (Context : in out Context_Type) return String;

   type Command_Type is abstract new Main_Driver.Command_Type with null record;

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

   type Application_Command_Type is abstract new Command_Type with record
      Application_Name : aliased GNAT.Strings.String_Access;
   end record;

   function Is_Application (Command : in Application_Command_Type;
                            URI     : in String) return Boolean;

   procedure Execute (Command     : in out Application_Command_Type;
                      Application : in out AWA.Applications.Application'Class;
                      Args        : in Argument_List'Class;
                      Context     : in out Context_Type) is abstract;

   --  Setup the command before parsing the arguments and executing it.
   overriding
   procedure Setup (Command : in out Application_Command_Type;
                    Config  : in out GNAT.Command_Line.Command_Line_Configuration;
                    Context : in out Context_Type);

   overriding
   procedure Execute (Command   : in out Application_Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type);

   --  Print the command usage.
   procedure Usage (Args    : in Argument_List'Class;
                    Context : in out Context_Type;
                    Name    : in String := "");

   --  Execute the command with its arguments.
   procedure Execute (Name    : in String;
                      Args    : in Argument_List'Class;
                      Context : in out Context_Type);

   procedure Run (Context   : in out Context_Type;
                  Arguments : out Util.Commands.Dynamic_Argument_List);

   Driver   : Drivers.Driver_Type;
   WS       : Container_Type;

end AWA.Commands.Drivers;
