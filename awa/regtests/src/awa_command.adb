-----------------------------------------------------------------------
--  awa_command - Tests for AWA command
--  Copyright (C) 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Commands;
with Util.Tests;
with AWA.Commands.Drivers;
with AWA.Commands.List;
with AWA.Commands.Start;
with AWA.Commands.Stop;
with AWA.Commands.Info;
with AWA.Commands.User;
with AWA.Commands.Migrate;
with Servlet.Server;
with ADO.Drivers;
with AWA.Testsuite;
with AWA_Test_App;
procedure AWA_Command is

   package Server_Commands is
     new AWA.Commands.Drivers (Driver_Name => "awa",
                               Container_Type => Servlet.Server.Container);

   package List_Command is
      new AWA.Commands.List (Server_Commands);

   package Start_Command is
      new AWA.Commands.Start (Server_Commands);

   package Stop_Command is
      new AWA.Commands.Stop (Server_Commands);

   package Info_Command is
      new AWA.Commands.Info (Server_Commands);

   package User_Command is
      new AWA.Commands.User (Server_Commands);

   package Migrate_Command is
      new AWA.Commands.Migrate (Server_Commands);
   pragma Unreferenced (List_Command, Start_Command, Stop_Command, Info_Command,
                        User_Command, Migrate_Command);

   App       : aliased AWA_Test_App.Application;
   Context   : AWA.Commands.Context_Type;
   Arguments : Util.Commands.Dynamic_Argument_List;
   Suite     : Util.Tests.Access_Test_Suite := AWA.Testsuite.Suite;
   pragma Unreferenced (Suite);
begin
   ADO.Drivers.Initialize;
   Server_Commands.WS.Register_Application ("/test", App'Unchecked_Access);
   Server_Commands.Run (Context, Arguments);

exception
   when E : others =>
      AWA.Commands.Print (Context, E);

end AWA_Command;
