-----------------------------------------------------------------------
--  awa-commands-stop -- Command to stop the web server
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
with GNAT.Sockets;
with Util.Streams.Sockets;
with Util.Streams.Texts;
package body AWA.Commands.Stop is

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
      GC.Define_Switch (Config => Config,
                        Output => Command.Management_Port'Access,
                        Switch => "-m:",
                        Long_Switch => "--management-port=",
                        Initial  => Command.Management_Port,
                        Argument => "NUMBER",
                        Help   => -("The server listening management port on localhost"));
      AWA.Commands.Setup_Command (Config, Context);
   end Setup;

   --  ------------------------------
   --  Stop the server by sending a 'stop' command on the management socket.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Name, Args, Context);

      Address : GNAT.Sockets.Sock_Addr_Type;
      Stream  : aliased Util.Streams.Sockets.Socket_Stream;
      Writer  : Util.Streams.Texts.Print_Stream;
   begin
      Address.Addr := GNAT.Sockets.Loopback_Inet_Addr;
      Address.Port := GNAT.Sockets.Port_Type (Command.Management_Port);
      Stream.Connect (Address);
      Writer.Initialize (Stream'Unchecked_Access, 1024);
      Writer.Write ("stop" & ASCII.CR & ASCII.LF);
      Writer.Flush;
      Writer.Close;
   end Execute;

begin
   Command_Drivers.Driver.Add_Command ("stop",
                                       -("stop the web server"),
                                       Command'Access);
end AWA.Commands.Stop;
