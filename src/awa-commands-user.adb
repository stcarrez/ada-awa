-----------------------------------------------------------------------
--  awa-commands-user -- Simple user management
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
   procedure Execute (Command     : in out Command_Type;
                      Application : in out AWA.Applications.Application'Class;
                      Args        : in Argument_List'Class;
                      Context     : in out Context_Type) is
      User_Module : AWA.Modules.Module_Access;
      Service     : AWA.Users.Services.User_Service_Access;
   begin
      Application.Load_Bundle (Name   => "commands",
                               Locale => "en",
                               Bundle => Command.Bundle);
      User_Module := Application.Find_Module (AWA.Users.Modules.NAME);
      if User_Module = null then
         Context.Console.Error ("There is no user manager in this application");
         return;
      end if;

      Service := AWA.Users.Modules.User_Module'Class (User_Module.all).Get_User_Manager;
      if Args.Get_Count /= 1 then
         Context.Console.Error ("Invalid arguments for command: expecting a mail address");
         return;
      end if;

      if not Command.Register and then not Command.Enable and then not Command.Disable then
         Context.Console.Error ("Use one of --register, --enable or --disable option");
         return;
      end if;
      Service.Set_Allow_Register (True);

      declare
         Service_Context : aliased AWA.Services.Contexts.Service_Context;
         Param : constant String := Args.Get_Argument (1);
         Addr  : constant Util.Mail.Email_Address := Util.Mail.Parse_Address (Param);
         Email : AWA.Users.Models.Email_Ref;
         User  : AWA.Users.Models.User_Ref;
         Key   : AWA.Users.Models.Access_Key_Ref;
      begin
         Service_Context.Set_Context (Application'Unchecked_Access, null);
         Email.Set_Email (Param);
         User.Set_First_Name (Util.Mail.Get_First_Name (Addr));
         User.Set_Last_Name (Util.Mail.Get_Last_Name (Addr));
         if Command.Register then
            Service.Create_User (User, Email, "", Key, not Command.No_Email);
            Context.Console.Notice (N_INFO, "User '" & Param & "' is now registered");
            Context.Console.Notice (N_INFO, "Registration key: "
                                      & String '(Key.Get_Access_Key));
         elsif Command.Enable then
            Service.Update_User (Param, AWA.Users.Models.USER_ENABLED);
         elsif Command.Disable then
            Service.Update_User (Param, AWA.Users.Models.USER_DISABLED);
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
                        Output => Command.No_Email'Access,
                        Long_Switch => "--no-email",
                        Help   => -("Don't send an email to the new user"));
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
