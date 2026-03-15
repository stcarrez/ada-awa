-----------------------------------------------------------------------
--  awa-commands-permission -- Permission command to list, add or remove permissions
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Strings;
with ADO.Sessions;
with ADO.Statements;
with ADO.Queries;
with AWA.Commands.Models;
package body AWA.Commands.Permission is

   procedure List (Command : in out Command_Type;
                   Context : in out Context_Type;
                   Session : in out ADO.Sessions.Master_Session;
                   Email   : in String);

   function Image (Id : in ADO.Identifier) return String is
      (Util.Strings.Image (Long_Long_Integer (Id)));

   --  ------------------------------
   --  List some database information.
   --  ------------------------------
   overriding
   procedure Execute (Command     : in out Command_Type;
                      Application : in out AWA.Applications.Application'Class;
                      Args        : in Argument_List'Class;
                      Context     : in out Context_Type) is
      Session     : ADO.Sessions.Master_Session;
   begin
      Application.Load_Bundle (Name   => "commands",
                               Locale => "en",
                               Bundle => Command.Bundle);
      Session := Application.Get_Master_Session;

      if Command.Allow_Permission'Length = 0 and then Command.Deny_Permission'Length = 0 then
         List (Command, Context, Session, Args.Get_Argument (1));
      end if;
   end Execute;

   procedure List (Command : in out Command_Type;
                   Context : in out Context_Type;
                   Session : in out ADO.Sessions.Master_Session;
                   Email   : in String) is
      Query       : ADO.Queries.Context;
      Statement   : ADO.Statements.Query_Statement;
   begin
      Query.Set_Query (AWA.Commands.Models.Query_Command_User_Permissions);
      Query.Bind_Param ("email", Email);
      Statement := Session.Create_Statement (Query);
      Statement.Execute;

      Context.Console.Start_Title;
      Context.Console.Print_Title (1, "ACL", 8);
      Context.Console.Print_Title (2, "Permission:Workspace:Type:Entity", 60);
      Context.Console.End_Title;

      while Statement.Has_Elements loop
         declare
            Id          : constant ADO.Identifier := Statement.Get_Identifier (0);
            Ws_Id       : constant ADO.Identifier := Statement.Get_Identifier (1);
            Entity_Id   : constant ADO.Identifier := Statement.Get_Identifier (2);
            Entity_Type : constant String := Statement.Get_String (4);
            Perm        : constant String := Statement.Get_String (5);
         begin
            Context.Console.Start_Row;
            Context.Console.Print_Field (1, Image (Id));
            Context.Console.Print_Field (2, Perm & ":" & Image (Ws_Id) & ":" & Entity_Type
                                         & ":" & Image (Entity_Id));
            Context.Console.End_Row;
            Statement.Next;
         end;
      end loop;
   end List;

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
                        Output => Command.Allow_Permission'Access,
                        Switch => "-a:",
                        Long_Switch => "--allow=",
                        Argument => "PERMISSION",
                        Help   => -("Add a permission for the user"));
      GC.Define_Switch (Config => Config,
                        Output => Command.Deny_Permission'Access,
                        Switch => "-d:",
                        Long_Switch => "--deny=",
                        Argument => "PERMISSION",
                        Help   => -("Remove a permission for the user"));
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
   Command_Drivers.Driver.Add_Command ("permission",
                                       -("manage user permissions"),
                                       Command'Access);
end AWA.Commands.Permission;
