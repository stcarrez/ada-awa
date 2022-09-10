-----------------------------------------------------------------------
--  akt-commands-list -- List commands to report database information for admin tool
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
with Util.Strings;
with ADO.Sessions;
with ADO.Statements;
with ADO.Schemas;
with ADO.Queries;
with AWA.Commands.Models;
package body AWA.Commands.List is

   procedure List (Command : in out Command_Type;
                   Context : in out Context_Type;
                   Session : in out ADO.Sessions.Master_Session;
                   Query   : in ADO.Queries.Context;
                   Prefix  : in String);

   procedure Database_Summary (Command : in out Command_Type;
                               Context : in out Context_Type;
                               Session : in out ADO.Sessions.Master_Session);

   --  ------------------------------
   --  List some database information.
   --  ------------------------------
   overriding
   procedure Execute (Command     : in out Command_Type;
                      Application : in out AWA.Applications.Application'Class;
                      Args        : in Argument_List'Class;
                      Context     : in out Context_Type) is
      pragma Unreferenced (Args);

      Session     : ADO.Sessions.Master_Session;
      Query       : ADO.Queries.Context;
   begin
      Application.Load_Bundle (Name   => "commands",
                               Locale => "en",
                               Bundle => Command.Bundle);
      Session := Application.Get_Master_Session;

      if Command.List_Users then
         Query.Set_Query (AWA.Commands.Models.Query_Command_User_List);
         List (Command, Context, Session, Query, "command_user_list_");
      end if;

      if Command.List_Sessions then
         Query.Set_Query (AWA.Commands.Models.Query_Command_Session_List);
         List (Command, Context, Session, Query, "command_session_list_");
      end if;

      if Command.List_Jobs then
         Query.Set_Query (AWA.Commands.Models.Query_Command_Job_List);
         List (Command, Context, Session, Query, "command_job_list_");
      end if;

      if Command.List_Audits then
         Query.Set_Query (AWA.Commands.Models.Query_Command_Audit_List);
         List (Command, Context, Session, Query, "command_audit_list_");
      end if;

      if Command.List_Tables then
         Database_Summary (Command, Context, Session);
      end if;
   end Execute;

   procedure List (Command : in out Command_Type;
                   Context : in out Context_Type;
                   Session : in out ADO.Sessions.Master_Session;
                   Query   : in ADO.Queries.Context;
                   Prefix  : in String) is
      function Get_Label (Name : in String) return String;

      function Get_Label (Name : in String) return String is
         Result : constant String := Command.Bundle.Get (Prefix & Name);
      begin
         if Util.Strings.Index (Result, ':') > 0 then
            return Result;
         else
            return Result & ":10";
         end if;
      end Get_Label;

      Statement   : ADO.Statements.Query_Statement;
      Print_Names : Boolean := True;
   begin
      Statement := Session.Create_Statement (Query);
      Statement.Execute;
      while Statement.Has_Elements loop
         if Print_Names then
            Context.Console.Start_Title;
            for I in 1 .. Statement.Get_Column_Count loop
               declare
                  Info : constant String := Get_Label (Statement.Get_Column_Name (I - 1));
                  Pos  : constant Natural := Util.Strings.Index (Info, ':');
               begin
                  Context.Console.Print_Title (Field_Number (I),
                                               Info (Info'First .. Pos - 1),
                                               Natural'Value (Info (Pos + 1 .. Info'Last)));
               end;
            end loop;
            Context.Console.End_Title;
            Print_Names := False;
         end if;
         Context.Console.Start_Row;
         for I in 1 .. Statement.Get_Column_Count loop
            if not Statement.Is_Null (I - 1) then
               Context.Console.Print_Field (Field_Number (I),
                                            Statement.Get_String (I - 1));
            end if;
         end loop;
         Context.Console.End_Row;
         Statement.Next;
      end loop;
   end List;

   procedure Database_Summary (Command : in out Command_Type;
                               Context : in out Context_Type;
                               Session : in out ADO.Sessions.Master_Session) is
      pragma Unreferenced (Command);
      use ADO.Schemas;

      Schema : ADO.Schemas.Schema_Definition;
      Iter   : Table_Cursor;
      Col    : Field_Number := 1;
   begin
      Session.Load_Schema (Schema);

      Context.Console.Start_Title;
      Context.Console.Print_Title (1, "Table", 30);
      Context.Console.Print_Title (2, "  Count", 8);
      Context.Console.Print_Title (3, "", 3);
      Context.Console.Print_Title (4, "Table", 30);
      Context.Console.Print_Title (5, "  Count", 8);
      Context.Console.End_Title;

      --  Dump the database schema using SQL create table forms.
      Iter := Get_Tables (Schema);
      while Has_Element (Iter) loop
         declare
            Table      : constant Table_Definition := Element (Iter);
            Count      : Natural;
            Statement  : ADO.Statements.Query_Statement;
         begin
            if Col = 1 then
               Context.Console.Start_Row;
            end if;
            Context.Console.Print_Field (Col, Get_Name (Table));
            Statement := Session.Create_Statement ("SELECT COUNT(*) FROM " & Get_Name (Table));
            Statement.Execute;
            Count := Statement.Get_Integer (0);
            Context.Console.Print_Field (Col + 1, Count, Consoles.J_RIGHT);
            if Col >= 3 then
               Context.Console.End_Row;
               Col := 1;
            else
               Context.Console.Print_Field (Col + 2, "");
               Col := Col + 3;
            end if;
         end;
         Next (Iter);
      end loop;
      if Col /= 1 then
         Context.Console.End_Row;
      end if;
   end Database_Summary;

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
                        Output => Command.List_Users'Access,
                        Switch => "-u",
                        Long_Switch => "--users",
                        Help   => -("List the users"));
      GC.Define_Switch (Config => Config,
                        Output => Command.List_Jobs'Access,
                        Switch => "-j",
                        Long_Switch => "--jobs",
                        Help   => -("List the jobs"));
      GC.Define_Switch (Config => Config,
                        Output => Command.List_Sessions'Access,
                        Switch => "-s",
                        Long_Switch => "--sessions",
                        Help   => -("List the sessions"));
      GC.Define_Switch (Config => Config,
                        Output => Command.List_Tables'Access,
                        Switch => "-t",
                        Long_Switch => "--tables",
                        Help   => -("List the database tables"));
      GC.Define_Switch (Config => Config,
                        Output => Command.List_Audits'Access,
                        Switch => "-A",
                        Long_Switch => "--audit",
                        Help   => -("List the last audit records"));
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
   Command_Drivers.Driver.Add_Command ("list",
                                       -("list information"),
                                       Command'Access);
end AWA.Commands.List;
