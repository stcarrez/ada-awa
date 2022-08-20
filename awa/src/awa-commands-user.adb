-----------------------------------------------------------------------
--  awa-commands-user -- Simple user management
--  Copyright (C) 2022 Stephane Carrez
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
with Util.Mail;
with AWA.Modules;
with AWA.Services.Contexts;
with AWA.Users.Models;
with AWA.Users.Modules;
with AWA.Users.Services;
package body AWA.Commands.User is

   use type AWA.Modules.Module_Access;

   --  ------------------------------
   --  Add, disable, enable a user.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
   begin
      Command_Drivers.Application_Command_Type (Command).Execute (Name, Args, Context);
   end Execute;

   overriding
   procedure Execute (Command     : in out Command_Type;
                      Application : in out AWA.Applications.Application'Class;
                      Args        : in Argument_List'Class;
                      Context     : in out Context_Type) is
      User_Module : constant AWA.Modules.Module_Access
         := Application.Find_Module (AWA.Users.Modules.NAME);
      Service     : AWA.Users.Services.User_Service_Access;
   begin
      Application.Load_Bundle (Name   => "commands",
                               Locale => "en",
                               Bundle => Command.Bundle);
      if User_Module = null then
         Context.Console.Error ("There is no user manager");
         return;
      end if;

      --  Enable and start the application and setup the service context.
      Application.Enable;
      Application.Start;

      Service := AWA.Users.Modules.User_Module'Class (User_Module.all).Get_User_Manager;

      if Args.Get_Count /= 1 then
         Context.Console.Error ("Invalid arguments for command: expecting a mail address");
         return;
      end if;
      declare
         Service_Context : aliased AWA.Services.Contexts.Service_Context;
         Param : constant String := Args.Get_Argument (1);
         Addr  : constant Util.Mail.Email_Address := Util.Mail.Parse_Address (Param);
         Email : AWA.Users.Models.Email_Ref;
         User  : AWA.Users.Models.User_Ref;
      begin
         Service_Context.Set_Context (Application'Unchecked_Access, null);
         Email.Set_Email (Param);
         User.Set_First_Name (Util.Mail.Get_First_Name (Addr));
         User.Set_Last_Name (Util.Mail.Get_Last_Name (Addr));
         if Command.Register then
            Service.Create_User (User, Email);
         end if;

      exception
         when AWA.Users.Services.User_Exist =>
            Context.Console.Error ("Email address '" & Param & "' is already registered");

      end;
   end Execute;

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
      Command_Drivers.Application_Command_Type (Command).Setup (Config, Context);
      GC.Define_Switch (Config => Config,
                        Output => Command.Register'Access,
                        Long_Switch => "--register",
                        Help   => -("Register a new user"));
      GC.Define_Switch (Config => Config,
                        Output => Command.Enable'Access,
                        Long_Switch => "--enable",
                        Help   => -("Enable the user"));
      GC.Define_Switch (Config => Config,
                        Output => Command.Disable'Access,
                        Long_Switch => "--disable",
                        Help   => -("Disable the user"));
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
   Command_Drivers.Driver.Add_Command ("user",
                                       -("simple user management"),
                                       Command'Access);
end AWA.Commands.User;
