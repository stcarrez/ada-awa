-----------------------------------------------------------------------
--  awa_command - Tests for AWA command
--  Copyright (C) 2020, 2022 Stephane Carrez
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
