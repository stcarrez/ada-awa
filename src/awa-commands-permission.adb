-----------------------------------------------------------------------
--  awa-commands-permission -- Permission command to list, add or remove permissions
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Util.Strings.Tokenizers;
with ADO.Sessions.Entities;
with ADO.Statements;
with ADO.Queries;
with ADO.SQL;
with ADO.Schemas.Entities;
with Security.Permissions;
with AWA.Commands.Models;
with AWA.Users.Models;
with AWA.Permissions.Services;
package body AWA.Commands.Permission is

   package SP renames Security.Permissions;

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   function To_UString (S : in String) return UString
                        renames Ada.Strings.Unbounded.To_Unbounded_String;
   function To_String (S : in UString) return String
                       renames Ada.Strings.Unbounded.To_String;

   Invalid_Permission : exception;

   procedure Split (Content      : in String;
                    Entity_Id    : out ADO.Identifier;
                    Workspace_Id : out ADO.Identifier;
                    Permission   : out UString;
                    Kind         : out UString);

   procedure List (Command : in out Command_Type;
                   Context : in out Context_Type;
                   Session : in out ADO.Sessions.Master_Session;
                   Args    : in Argument_List'Class);

   --  <server> permission --deny=blog-update-post:1:awa_blog:1 email...
   --  <server> permission --allow=blog-update-post:1:awa_blog:1 email...

   function Image (Id : in ADO.Identifier) return String is
      (Util.Strings.Image (Long_Long_Integer (Id)));

   generic
      with procedure Process (Session    : in out ADO.Sessions.Master_Session;
                              User       : in ADO.Identifier;
                              Entity     : in ADO.Identifier;
                              Kind       : in ADO.Entity_Type;
                              Workspace  : in ADO.Identifier;
                              Permission : in ADO.Identifier);
   procedure Permission_Command (Config   : in String;
                                 Context  : in out Context_Type;
                                 Security : in AWA.Permissions.Services.Permission_Manager_Access;
                                 Session  : in out ADO.Sessions.Master_Session;
                                 Args     : in Argument_List'Class);

   procedure Split (Content      : in String;
                    Entity_Id    : out ADO.Identifier;
                    Workspace_Id : out ADO.Identifier;
                    Permission   : out UString;
                    Kind         : out UString) is
      procedure Collect (Item : in String; Done : out Boolean);

      Pos : Natural := 0;

      procedure Collect (Item : in String; Done : out Boolean) is
      begin
         Pos := Pos + 1;
         if Pos = 1 then
            Permission := To_UString (Item);
         elsif Pos = 2 then
            Workspace_Id := ADO.Identifier'Value (Item);
         elsif Pos = 3 then
            Kind := To_UString (Item);
         elsif Pos = 4 then
            Entity_Id := ADO.Identifier'Value (Item);
         else
            raise Constraint_Error;
         end if;
         Done := False;
      end Collect;
   begin
      Entity_Id := ADO.NO_IDENTIFIER;
      Workspace_Id := ADO.NO_IDENTIFIER;
      Util.Strings.Tokenizers.Iterate_Tokens (Content, ":", Collect'Access);
   exception
      when Constraint_Error =>
         raise Invalid_Permission;
   end Split;

   procedure Permission_Command (Config   : in String;
                                 Context  : in out Context_Type;
                                 Security : in AWA.Permissions.Services.Permission_Manager_Access;
                                 Session  : in out ADO.Sessions.Master_Session;
                                 Args     : in Argument_List'Class) is
      Entity_Id    : ADO.Identifier;
      Workspace_Id : ADO.Identifier;
      Permission   : UString;
      Name_Kind    : UString;
   begin
      Split (Config, Entity_Id, Workspace_Id, Permission, Name_Kind);

      Security.Start;
      declare
         Kind : constant ADO.Entity_Type
           := ADO.Sessions.Entities.Find_Entity_Type (Session, To_String (Name_Kind));
         Perm : constant ADO.Identifier
           := Security.Get_Permission (To_String (Permission));
      begin
         for I in 1 .. Args.Get_Count loop
            declare
               Email_Address : constant String := Args.Get_Argument (I);
               Query : ADO.SQL.Query;
               Email : AWA.Users.Models.Email_Ref;
               Found : Boolean;
            begin
               Query.Set_Filter ("LOWER(o.email) = LOWER(?)");
               Query.Bind_Param (1, Email_Address);
               Email.Find (Session, Query, Found);
               if Found then
                  Session.Begin_Transaction;
                  Process (Session, Email.Get_User_Id, Workspace_Id, Kind, Entity_Id, Perm);
                  Session.Commit;
               else
                  Context.Console.Error ("User '" & Email_Address & "' was not found");
               end if;
            end;
         end loop;
      end;

   exception
      when Invalid_Permission =>
         Context.Console.Error ("Invalid permission: " & Config);
         Context.Console.Notice (N_INFO, "Format: <permission-name>:<workspace-id>"
                                 & ":<entity-type>:<entity-id>");

      when ADO.Schemas.Entities.No_Entity_Type =>
         Context.Console.Error ("Invalid entity type: " & To_String (Name_Kind));

      when SP.Invalid_Name =>
         Context.Console.Error ("Invalid permission: " & To_String (Permission));
   end Permission_Command;

   procedure Allow is
      new Permission_Command (AWA.Permissions.Services.Add_Permission);

   procedure Deny is
      new Permission_Command (AWA.Permissions.Services.Delete_Permission);

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

      if Command.Deny_Permission'Length > 0 then
         Deny (Command.Deny_Permission.all, Context,
               AWA.Permissions.Services.Get_Permission_Manager (Application),
               Session, Args);
      elsif Command.Allow_Permission'Length > 0 then
         Allow (Command.Allow_Permission.all, Context,
                AWA.Permissions.Services.Get_Permission_Manager (Application),
                Session, Args);
      elsif Command.Allow_Permission'Length = 0 and then Command.Deny_Permission'Length = 0 then
         List (Command, Context, Session, Args);
      end if;
   end Execute;

   procedure List (Command : in out Command_Type;
                   Context : in out Context_Type;
                   Session : in out ADO.Sessions.Master_Session;
                   Args    : in Argument_List'Class) is
      procedure List;

      Query       : ADO.Queries.Context;

      procedure List is
         Statement   : ADO.Statements.Query_Statement;
      begin
         Statement := Session.Create_Statement (Query);
         Statement.Execute;

         while Statement.Has_Elements loop
            declare
               Email       : constant String := Statement.Get_String (0);
               Id          : constant ADO.Identifier := Statement.Get_Identifier (1);
               Ws_Id       : constant ADO.Identifier := Statement.Get_Identifier (2);
               Entity_Id   : constant ADO.Identifier := Statement.Get_Identifier (3);
               Entity_Type : constant String := Statement.Get_String (5);
               Perm        : constant String := Statement.Get_String (6);
            begin
               Context.Console.Start_Row;
               Context.Console.Print_Field (1, Image (Id));
               Context.Console.Print_Field (2, Perm & ":" & Image (Ws_Id) & ":" & Entity_Type
                                            & ":" & Image (Entity_Id));
               Context.Console.Print_Field (3, Email);
               Context.Console.End_Row;
               Statement.Next;
            end;
         end loop;
      end List;
   begin
      Context.Console.Start_Title;
      Context.Console.Print_Title (1, "ACL", 8);
      Context.Console.Print_Title (2, "Permission:Workspace:Type:Entity", 60);
      Context.Console.Print_Title (3, "Email", 40);
      Context.Console.End_Title;

      if Args.Get_Count = 0 then
         Query.Set_Query (AWA.Commands.Models.Query_Command_All_Permissions);
         List;
      else
         Query.Set_Query (AWA.Commands.Models.Query_Command_User_Permissions);
         for I in 1 .. Args.Get_Count loop
            Query.Bind_Param ("email", Args.Get_Argument (I));
            List;
         end loop;
      end if;
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
