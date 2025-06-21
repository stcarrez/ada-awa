-----------------------------------------------------------------------
--  awa-commands-start -- Command to start the web server
--  Copyright (C) 2020, 2021, 2022, 2023, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with System;
with Ada.Strings.Unbounded;
with Servlet.Core;
with Servlet.Server;
with GNAT.Sockets;
with ASF.Applications.Main;
with AWA.Applications;
package body AWA.Commands.Start is

   use Ada.Strings.Unbounded;
   use GNAT.Sockets;
   use type System.Address;

   --  ------------------------------
   --  Start the server and all the application that have been registered.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
   begin
      if Args.Get_Count /= 0 then
         Command.Usage (Name, Context);
         return;
      end if;
      Command.Configure_Server (Context);
      Command.Configure_Applications (Context);
      Command.Start_Server (Context);
      Command.Wait_Server (Context);
   end Execute;

   --  ------------------------------
   --  Configure the web server container before applications are registered.
   --  ------------------------------
   procedure Configure_Server (Command   : in out Command_Type;
                               Context   : in out Context_Type) is
      Config  : Servlet.Server.Configuration;
   begin
      --  If daemon(3) is available and -d is defined, run it so that the parent
      --  process terminates and the child process continues.
      if Command.Daemon and then Sys_Daemon'Address /= System.Null_Address then
         declare
            Result : constant Integer := Sys_Daemon (1, 0);
         begin
            if Result /= 0 then
               Context.Console.Error ("Cannot run in background");
            end if;
         end;
      end if;

      Config.Listening_Port := Command.Listening_Port;
      Config.Max_Connection := Command.Max_Connection;
      Config.TCP_No_Delay := Command.TCP_No_Delay;
      Config.Input_Line_Size_Limit := Command.Input_Line_Size_Limit;
      Config.Upload_Size_Limit := Command.Upload_Size_Limit;
      if Command.Upload'Length > 0 then
         Config.Upload_Directory := To_Unbounded_String (Command.Upload.all);
      end if;
      Command_Drivers.WS.Configure (Config);
   end Configure_Server;

   --  ------------------------------
   --  Configure all registered applications.
   --  ------------------------------
   procedure Configure_Applications (Command   : in out Command_Type;
                                     Context   : in out Context_Type) is
      pragma Unreferenced (Command);

      procedure Configure (URI : in String;
                           Application : in Servlet.Core.Servlet_Registry_Access);

      Count : Natural := 0;

      procedure Configure (URI : in String;
                           Application : in Servlet.Core.Servlet_Registry_Access) is
      begin
         if Application.all in ASF.Applications.Main.Application'Class then
            Configure (URI (URI'First + 1 .. URI'Last), Context);
            ASF.Applications.Main.Application'Class (Application.all).Initialize
              (Context.App_Config, Context.Factory);
            Count := Count + 1;
         end if;
      end Configure;

   begin
      Command_Drivers.WS.Iterate (Configure'Access);
      if Count = 0 then
         Context.Console.Error (-("There is no application"));
         return;
      end if;
   end Configure_Applications;

   --  ------------------------------
   --  Start the web server.
   --  ------------------------------
   procedure Start_Server (Command   : in out Command_Type;
                           Context   : in out Context_Type) is
      pragma Unreferenced (Command);
   begin
      Context.Console.Notice (N_INFO, "Starting...");
      Command_Drivers.WS.Start;
   end Start_Server;

   --  ------------------------------
   --  Wait for the server to shutdown.
   --  ------------------------------
   procedure Wait_Server (Command   : in out Command_Type;
                          Context   : in out Context_Type) is
      pragma Unreferenced (Context);

      procedure Shutdown (URI : in String;
                          Application : in Servlet.Core.Servlet_Registry_Access);

      procedure Shutdown (URI : in String;
                          Application : in Servlet.Core.Servlet_Registry_Access) is
         pragma Unreferenced (URI);
      begin
         if Application.all in AWA.Applications.Application'Class then
            AWA.Applications.Application'Class (Application.all).Close;
         end if;
      end Shutdown;

      Address : GNAT.Sockets.Sock_Addr_Type;
      Listen  : GNAT.Sockets.Socket_Type;
      Socket  : GNAT.Sockets.Socket_Type;
   begin
      GNAT.Sockets.Create_Socket (Listen);
      Address.Addr := GNAT.Sockets.Loopback_Inet_Addr;
      if Command.Management_Port > 0 then
         Address.Port := Port_Type (Command.Management_Port);
      else
         Address.Port := 0;
      end if;
      GNAT.Sockets.Bind_Socket (Listen, Address);
      GNAT.Sockets.Listen_Socket (Listen);

      loop
         GNAT.Sockets.Accept_Socket (Listen, Socket, Address);
         exit;
      end loop;
      GNAT.Sockets.Close_Socket (Socket);
      GNAT.Sockets.Close_Socket (Listen);

      Command_Drivers.WS.Iterate (Shutdown'Access);
   end Wait_Server;

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
      GC.Define_Switch (Config => Config,
                        Output => Command.Listening_Port'Access,
                        Switch => "-p:",
                        Long_Switch => "--port=",
                        Initial  => Command.Listening_Port,
                        Argument => "NUMBER",
                        Help   => -("The server listening port"));
      GC.Define_Switch (Config => Config,
                        Output => Command.Max_Connection'Access,
                        Switch => "-C:",
                        Long_Switch => "--connection=",
                        Initial  => Command.Max_Connection,
                        Argument => "NUMBER",
                        Help   => -("The number of connections handled"));
      GC.Define_Switch (Config => Config,
                        Output => Command.Upload'Access,
                        Switch => "-u:",
                        Long_Switch => "--upload=",
                        Argument => "PATH",
                        Help   => -("The server upload directory"));
      GC.Define_Switch (Config => Config,
                        Output => Command.Upload'Access,
                        Switch => "-u:",
                        Long_Switch => "--upload=",
                        Argument => "PATH",
                        Help   => -("The server upload directory"));
      GC.Define_Switch (Config => Config,
                        Output => Command.Upload_Size_Limit'Access,
                        Switch => "-M:",
                        Long_Switch => "--max-upload-size=",
                        Argument => "SIZE",
                        Initial => Command.Upload_Size_Limit,
                        Help   => -("Maximum size of uploaded content"));
      GC.Define_Switch (Config => Config,
                        Output => Command.Input_Line_Size_Limit'Access,
                        Switch => "-F:",
                        Long_Switch => "--max-form-size=",
                        Argument => "SIZE",
                        Initial => Command.Input_Line_Size_Limit,
                        Help   => -("Maximum size of form submission"));
      if Sys_Daemon'Address /= System.Null_Address then
         GC.Define_Switch (Config => Config,
                           Output => Command.Daemon'Access,
                           Switch => "-d",
                           Long_Switch => "--daemon",
                           Help   => -("Run the server in the background"));
      end if;
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

begin
   Command_Drivers.Driver.Add_Command ("start",
                                       -("start the web server"),
                                       Command'Access);
end AWA.Commands.Start;
