-----------------------------------------------------------------------
--  awa-setup -- Setup and installation
--  Copyright (C) 2016 Stephane Carrez
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
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Directories;
with Util.Files;
with Util.Log.Loggers;
with Util.Strings;
with ASF.Events.Faces.Actions;
with ASF.Applications.Main.Configs;
with AWA.Applications;
package body AWA.Setup.Applications is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Setup.Applications");

   package Save_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Application,
                                                      Method => Save,
                                                      Name   => "save");

   package Finish_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Application,
                                                      Method => Finish,
                                                      Name   => "finish");
   package Configure_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Application,
                                                      Method => Configure_Database,
                                                      Name   => "configure_database");


   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Save_Binding.Proxy'Access,
         Finish_Binding.Proxy'Access,
         Configure_Binding.Proxy'Access);

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   function Get_Value (From : in Application;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "database_name" then
         return Util.Beans.Objects.To_Object (From.Database.Get_Database);
      elsif Name = "database_server" then
         return Util.Beans.Objects.To_Object (From.Database.Get_Server);
      elsif Name = "database_port" then
         return Util.Beans.Objects.To_Object (From.Database.Get_Port);
      elsif Name = "database_user" then
         return Util.Beans.Objects.To_Object (From.Database.Get_Property ("user"));
      elsif Name = "database_password" then
         return Util.Beans.Objects.To_Object (From.Database.Get_Property ("password"));
      end if;
      if From.Changed.Exists (Name) then
         return Util.Beans.Objects.To_Object (String '(From.Changed.Get (Name)));
      end if;
      declare
         Param : constant String := From.Config.Get (Name);
      begin
         return Util.Beans.Objects.To_Object (Param);
      end;

   exception
      when others =>
         return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   procedure Set_Value (From  : in out Application;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "database_name" then
         From.Database.Set_Database (Util.Beans.Objects.To_String (Value));
      elsif Name = "database_server" then
         From.Database.Set_Server (Util.Beans.Objects.To_String (Value));
      elsif Name = "database_port" then
         From.Database.Set_Port (Util.Beans.Objects.To_Integer (Value));
      elsif Name = "database_user" then
         From.Database.Set_Property ("user", Util.Beans.Objects.To_String (Value));
      elsif Name = "database_password" then
         From.Database.Set_Property ("password", Util.Beans.Objects.To_String (Value));
      elsif Name = "callback_url" then
         From.Changed.Set (Name, Util.Beans.Objects.To_String (Value));
         From.Changed.Set ("facebook.callback_url",
                           Util.Beans.Objects.To_String (Value) & "#{contextPath}/auth/verify");
         From.Changed.Set ("google-plus.callback_url",
                           Util.Beans.Objects.To_String (Value) & "#{contextPath}/auth/verify");
      else
         From.Changed.Set (Name, Util.Beans.Objects.To_String (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Get the database connection string to be used by the application.
   --  ------------------------------
   function Get_Database_URL (From : in Application) return String is
      use Ada.Strings.Unbounded;

      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Result, "mysql://");
      Append (Result, From.Database.Get_Server);
      if From.Database.Get_Port /= 0 then
         Append (Result, ":");
         Append (Result, Util.Strings.Image (From.Database.Get_Port));
      end if;
      Append (Result, "/");
      Append (Result, From.Database.Get_Database);
      if From.Database.Get_Property ("user") /= "" then
         Append (Result, "?user=");
         Append (Result, From.Database.Get_Property ("user"));
         if From.Database.Get_Property ("password") /= "" then
            Append (Result, "&password=");
            Append (Result, From.Database.Get_Property ("password"));
         end if;
      end if;
      return To_String (Result);
   end Get_Database_URL;

   --  Configure the database.
   procedure Configure_Database (From    : in out Application;
                                 Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Log.Info ("Configure database");
   end Configure_Database;

   --  ------------------------------
   --  Save the configuration.
   --  ------------------------------
   procedure Save (From    : in out Application;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Path     : constant String := Ada.Strings.Unbounded.To_String (From.Path);
      New_File : constant String := Path & ".tmp";
      Output   : Ada.Text_IO.File_Type;

      procedure Read_Property (Line : in String) is
         Pos : constant Natural := Util.Strings.Index (Line, '=');
      begin
         if Pos = 0 or else not From.Changed.Exists (Line (Line'First .. Pos - 1)) then
            Ada.Text_IO.Put_Line (Output, Line);
            return;
         end if;
         Ada.Text_IO.Put (Output, Line (Line'First .. Pos));
         Ada.Text_IO.Put_Line (Output, From.Changed.Get (Line (Line'First .. Pos - 1)));
      end Read_Property;

   begin
      Log.Info ("Saving configuration file {0}", Path);

      From.Changed.Set ("database", From.Get_Database_URL);
      Ada.Text_IO.Create (File => Output, Name => New_File);
      Util.Files.Read_File (Path, Read_Property'Access);
      Ada.Text_IO.Close (Output);
      Ada.Directories.Delete_File (Path);
      Ada.Directories.Rename (Old_Name => New_File,
                              New_Name => Path);
   end Save;

   --  Finish the setup and exit the setup.
   procedure Finish (From    : in out Application;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Log.Info ("Finish configuration");
      From.Done := True;
   end Finish;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Application)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Enter in the application setup
   --  ------------------------------
   procedure Setup (App    : in out Application;
                    Config : in String;
                    Server : in out ASF.Server.Container'Class) is
   begin
      Log.Info ("Entering configuration for {0}", Config);
      App.Path := Ada.Strings.Unbounded.To_Unbounded_String (Config);
      begin
         App.Config.Load_Properties (Config);
         Util.Log.Loggers.Initialize (Config);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Cannot read application configuration file: {0}", Config);
      end;
      App.Initialize (App.Config, App.Factory);
      App.Set_Global ("contextPath", App.Config.Get ("contextPath"));
      App.Set_Global ("setup",
                      Util.Beans.Objects.To_Object (App'Unchecked_Access,
                        Util.Beans.Objects.STATIC));
      App.Add_Servlet (Name   => "faces",
                       Server => App.Faces'Unchecked_Access);
      App.Add_Servlet (Name   => "files",
                       Server => App.Files'Unchecked_Access);
      App.Add_Mapping (Pattern => "*.html",
                       Name    => "faces");
      App.Add_Mapping (Pattern => "*.css",
                       Name    => "files");
      App.Add_Mapping (Pattern => "*.js",
                       Name    => "files");

      declare
         Paths : constant String := App.Get_Config (AWA.Applications.P_Module_Dir.P);
         Path  : constant String := Util.Files.Find_File_Path ("setup.xml", Paths);
      begin
         ASF.Applications.Main.Configs.Read_Configuration (App, Path);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Setup configuration file '{0}' does not exist", Path);
      end;

      declare
         URL : constant String := App.Config.Get ("google-plus.callback_url", "");
         Pos : Natural := Util.Strings.Index (URL, '#');
      begin
         if Pos > 0 then
            App.Changed.Set ("callback_url", URL (URL'First .. Pos - 1));
         end if;
      end;
      App.Database.Set_Connection (App.Config.Get ("database", "mysql://localhost:3306/db"));
      Server.Register_Application (App.Config.Get ("contextPath"), App'Unchecked_Access);
      while not App.Done loop
         delay 5.0;
      end loop;
      Server.Remove_Application (App'Unchecked_Access);
   end Setup;

end AWA.Setup.Applications;
