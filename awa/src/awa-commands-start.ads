-----------------------------------------------------------------------
--  awa-commands-start -- Command to start the web server
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
with AWA.Commands.Drivers;
generic
   with package Command_Drivers is new AWA.Commands.Drivers (<>);
package AWA.Commands.Start is

   type Command_Type is new Command_Drivers.Command_Type with record
      Management_Port       : aliased Integer := 0;
      Listening_Port        : aliased Integer := 8080;
      Upload_Size_Limit     : aliased Integer := 16#500_000#;
      Input_Line_Size_Limit : aliased Integer := 16#10000#;
      Max_Connection        : aliased Integer := 5;
      TCP_No_Delay          : aliased Boolean := False;
      Daemon                : aliased Boolean := False;
      Upload                : aliased GNAT.Strings.String_Access;
   end record;

   --  Start the server and all the application that have been registered.
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type);

   --  Configure the web server container before applications are registered.
   procedure Configure_Server (Command   : in out Command_Type;
                               Context   : in out Context_Type);

   --  Configure all registered applications.
   procedure Configure_Applications (Command   : in out Command_Type;
                                     Context   : in out Context_Type);

   --  Start the web server.
   procedure Start_Server (Command   : in out Command_Type;
                           Context   : in out Context_Type);

   --  Wait for the server to shutdown.
   procedure Wait_Server (Command   : in out Command_Type;
                          Context   : in out Context_Type);

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

   Command      : aliased Command_Type;

end AWA.Commands.Start;
