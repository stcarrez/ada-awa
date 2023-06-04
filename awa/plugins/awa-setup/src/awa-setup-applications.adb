-----------------------------------------------------------------------
--  awa-setup -- Setup and installation
--  Copyright (C) 2016, 2017, 2018, 2020, 2022, 2023 Stephane Carrez
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
with Util.Properties;
with Util.Log.Loggers;
with Util.Strings.Vectors;
with Util.Strings;
with ADO.Configs;
with ADO.Sessions.Sources;
with ADO.Schemas.Databases;
with ASF.Events.Faces.Actions;
with ASF.Applications.Main.Configs;
with ASF.Applications.Messages.Factory;
with AWA.Applications;
with AWA.Applications.Configs;
with AWA.Components.Factory;
package body AWA.Setup.Applications is

   use ASF.Applications;
   use Ada.Strings.Unbounded;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Setup.Applications");

   package Save_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Application,
                                                      Method => Save,
                                                      Name   => "save");
   package Start_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Application,
                                                      Method => Start,
                                                      Name   => "start");

   package Finish_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Application,
                                                      Method => Finish,
                                                      Name   => "finish");
   package Configure_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Application,
                                                      Method => Configure_Database,
                                                      Name   => "configure_database");

   --  The application base URL.
   package P_Base_URL is
     new ASF.Applications.Main.Configs.Parameter ("app_url_base",
                                                  "http://localhost:8080/#{contextPath}");

   package P_Dynamo_Path is
     new ASF.Applications.Main.Configs.Parameter ("dynamo_path",
                                                  "dynamo");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Save_Binding.Proxy'Access,
         Start_Binding.Proxy'Access,
         Finish_Binding.Proxy'Access,
         Configure_Binding.Proxy'Access);

   function Get_Schema_Path (Model_Dir : in String;
                             Model     : in String;
                             Config    : in ADO.Sessions.Sources.Data_Source) return String;
   function Get_Model_Directory (From : in Application) return String;
   function Get_Model_Name (From : in Application) return String;

   function Get_Schema_Path (Model_Dir : in String;
                             Model     : in String;
                             Config    : in ADO.Sessions.Sources.Data_Source) return String is
      Driver : constant String := Config.Get_Driver;
      Dir    : constant String := Util.Files.Compose (Model_Dir, Driver);
   begin
      return Util.Files.Compose (Dir, "create-" & Model & "-" & Driver & ".sql");
   end Get_Schema_Path;

   protected body State is
      --  ------------------------------
      --  Wait until the configuration is finished.
      --  ------------------------------
      entry Wait_Configuring when Value /= CONFIGURING is
      begin
         null;
      end Wait_Configuring;

      --  ------------------------------
      --  Wait until the server application is initialized and ready.
      --  ------------------------------
      entry Wait_Ready when Value = READY is
      begin
         null;
      end Wait_Ready;

      --  ------------------------------
      --  Set the configuration state.
      --  ------------------------------
      procedure Set (V : in Configure_State) is
      begin
         Value := V;
      end Set;

   end State;

   overriding
   procedure Do_Get (Server   : in Redirect_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class) is
      pragma Unreferenced (Server);

      Context_Path : constant String := Request.Get_Context_Path;
   begin
      Response.Send_Redirect (Context_Path & "/setup/install.html");
   end Do_Get;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Application;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "database_name" then
         return Util.Beans.Objects.To_Object (From.Database.Get_Database);
      elsif Name = "database_server" then
         return From.Db_Host;
      elsif Name = "database_port" then
         return From.Db_Port;
      elsif Name = "database_user" then
         return Util.Beans.Objects.To_Object (From.Database.Get_Property ("user"));
      elsif Name = "database_password" then
         return Util.Beans.Objects.To_Object (From.Database.Get_Property ("password"));
      elsif Name = "database_driver" then
         return From.Driver;
      elsif Name = "database_root_user" then
         return From.Root_User;
      elsif Name = "database_root_password" then
         return From.Root_Passwd;
      elsif Name = "result" then
         return From.Result;
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
   overriding
   procedure Set_Value (From  : in out Application;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "database_name" then
         From.Database.Set_Database (Util.Beans.Objects.To_String (Value));
      elsif Name = "database_server" then
         From.Db_Host := Value;
      elsif Name = "database_port" then
         From.Db_Port := Value;
      elsif Name = "database_user" then
         From.Database.Set_Property ("user", Util.Beans.Objects.To_String (Value));
      elsif Name = "database_password" then
         From.Database.Set_Property ("password", Util.Beans.Objects.To_String (Value));
      elsif Name = "database_driver" then
         From.Driver := Value;
      elsif Name = "database_root_user" then
         From.Root_User := Value;
      elsif Name = "database_root_password" then
         From.Root_Passwd := (if Util.Beans.Objects.Is_Null (Value) then Empty else Value);
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
      Result   : Ada.Strings.Unbounded.Unbounded_String;
      Driver   : constant String := Util.Beans.Objects.To_String (From.Driver);
      User     : constant String := From.Database.Get_Property ("user");
   begin
      if Driver = "" then
         Append (Result, "mysql");
      else
         Append (Result, Driver);
      end if;
      Append (Result, "://");
      if Driver /= "sqlite" then
         Append (Result, From.Database.Get_Server);
         if From.Database.Get_Port /= 0 then
            Append (Result, ":");
            Append (Result, Util.Strings.Image (From.Database.Get_Port));
         end if;
      end if;
      Append (Result, "/");
      Append (Result, From.Database.Get_Database);
      if User /= "" then
         Append (Result, "?user=");
         Append (Result, User);
         if From.Database.Get_Property ("password") /= "" then
            Append (Result, "&password=");
            Append (Result, From.Database.Get_Property ("password"));
         end if;
      end if;
      if Driver = "sqlite" then
         if User /= "" then
            Append (Result, "&");
         else
            Append (Result, "?");
         end if;
         Append (Result, "synchronous=OFF&encoding=UTF-8");
      end if;
      return To_String (Result);
   end Get_Database_URL;

   function Get_Model_Directory (From : in Application) return String is
      pragma Unreferenced (From);
   begin
      return "db";
   end Get_Model_Directory;

   function Get_Model_Name (From : in Application) return String is
   begin
      return To_String (From.Name);
   end Get_Model_Name;

   --  ------------------------------
   --  Get the command to configure the database.
   --  ------------------------------
   function Get_Configure_Command (From : in Application) return String is
      Database : constant String := From.Get_Database_URL;
      Dynamo   : constant String := From.Get_Config (P_Dynamo_Path.P);
      Command  : constant String := Dynamo & " create-database db '" & Database & "'";
      Root     : constant String := Util.Beans.Objects.To_String (From.Root_User);
      Passwd   : constant String := Util.Beans.Objects.To_String (From.Root_Passwd);
   begin
      if Root = "" then
         return Command;
      elsif Passwd = "" then
         return Command & " " & Root;
      else
         return Command & " " & Root & " " & Passwd;
      end if;
   end Get_Configure_Command;

   --  ------------------------------
   --  Validate the database configuration parameters.
   --  ------------------------------
   procedure Validate (From : in out Application) is
      Driver   : constant String := Util.Beans.Objects.To_String (From.Driver);
      Server   : constant String := Util.Beans.Objects.To_String (From.Db_Host);
   begin
      From.Has_Error := False;
      if Driver = "sqlite" then
         return;
      end if;
      begin
         From.Database.Set_Port (Util.Beans.Objects.To_Integer (From.Db_Port));

      exception
         when others =>
            From.Has_Error := True;
            Messages.Factory.Add_Field_Message ("db-port", "setup.setup_database_port_error",
                                                Messages.ERROR);

      end;
      if Server'Length = 0 then
         From.Has_Error := True;
         Messages.Factory.Add_Field_Message ("db-server", "setup.setup_database_host_error",
                                             Messages.ERROR);
      end if;
      From.Database.Set_Server (Server);
   end Validate;

   --  ------------------------------
   --  Configure the database.
   --  ------------------------------
   procedure Configure_Database (From    : in out Application;
                                 Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Admin     : ADO.Sessions.Sources.Data_Source;
      Config    : ADO.Sessions.Sources.Data_Source;
   begin
      From.Validate;
      if not From.Has_Error then
         Config.Set_Connection (From.Get_Database_URL);
         Admin := Config;
         declare
            Msgs      : Util.Strings.Vectors.Vector;
            Name      : constant String := From.Get_Model_Name;
            Model_Dir : constant String := From.Get_Model_Directory;
            Path      : constant String := Get_Schema_Path (Model_Dir, Name, Config);
            Content   : Ada.Strings.Unbounded.Unbounded_String;
         begin
            Log.Info ("Creating database tables using schema '{0}'", Path);

            if not Ada.Directories.Exists (Path) then
               Log.Error ("SQL file '{0}' does not exist.", Path);
               Log.Error ("Please, run the following command: dynamo generate db");
               Messages.Factory.Add_Message ("setup.setup_database_error", Messages.ERROR);
               Ada.Strings.Unbounded.Set_Unbounded_String (Outcome, "failure");
               return;
            end if;

            if Config.Get_Driver in "mysql" | "postgresql" then
               Admin.Set_Property ("user", Ubo.To_String (From.Root_User));
               Admin.Set_Property ("password", Ubo.To_String (From.Root_Passwd));

            elsif Config.Get_Driver /= "sqlite" then
               Log.Error ("Database driver {0} is not supported.", Config.Get_Driver);
               return;
            end if;
            Admin.Set_Database ("");
            ADO.Schemas.Databases.Create_Database (Admin, Config, Path, Msgs);
            for Line of Msgs loop
               Append (Content, Line);
            end loop;
            From.Result := Util.Beans.Objects.To_Object (Content);
            From.Has_Error := not Msgs.Is_Empty;

         exception
            when ADO.Configs.Connection_Error =>
               Messages.Factory.Add_Message ("setup.setup_database_error", Messages.ERROR);

         end;
         if From.Has_Error then
            Messages.Factory.Add_Message ("setup.setup_database_error", Messages.ERROR);
         end if;
      end if;
      if From.Has_Error then
         Ada.Strings.Unbounded.Set_Unbounded_String (Outcome, "failure");
      end if;
   end Configure_Database;

   --  ------------------------------
   --  Save the configuration.
   --  ------------------------------
   procedure Save (From    : in out Application;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      procedure Save_Property (Name  : in String;
                               Value : in Util.Properties.Value);
      procedure Read_Property (Line : in String);

      Path     : constant String := Ada.Strings.Unbounded.To_String (From.Path);
      New_File : constant String := Path & ".tmp";
      Output   : Ada.Text_IO.File_Type;
      Changed  : ASF.Applications.Config := From.Changed;

      procedure Read_Property (Line : in String) is
         Pos : constant Natural := Util.Strings.Index (Line, '=');
      begin
         if Pos = 0 or else not Changed.Exists (Line (Line'First .. Pos - 1)) then
            Ada.Text_IO.Put_Line (Output, Line);
            return;
         end if;
         Ada.Text_IO.Put (Output, Line (Line'First .. Pos));
         Ada.Text_IO.Put_Line (Output, Changed.Get (Line (Line'First .. Pos - 1)));
         Changed.Remove (Line (Line'First .. Pos - 1));
      end Read_Property;

      procedure Save_Property (Name  : in String;
                               Value : in Util.Properties.Value) is
      begin
         Ada.Text_IO.Put (Output, Name);
         Ada.Text_IO.Put (Output, "=");
         Ada.Text_IO.Put_Line (Output, Util.Properties.To_String (Value));
      end Save_Property;

   begin
      Log.Info ("Saving configuration file {0}", Path);

      Changed.Set ("database", From.Get_Database_URL);
      Ada.Text_IO.Create (File => Output, Name => New_File);
      Util.Files.Read_File (Path, Read_Property'Access);
      Changed.Iterate (Save_Property'Access);
      Ada.Text_IO.Close (Output);
      Ada.Directories.Delete_File (Path);
      Ada.Directories.Rename (Old_Name => New_File,
                              New_Name => Path);
   end Save;

   --  ------------------------------
   --  Finish the setup to start the application.
   --  ------------------------------
   procedure Start (From    : in out Application;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      From.Status.Set (STARTING);
      Log.Info ("Waiting for application to be started");
      From.Status.Wait_Ready;
   end Start;

   --  ------------------------------
   --  Finish the setup and exit the setup.
   --  ------------------------------
   procedure Finish (From    : in out Application;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Log.Info ("Finish configuration");
      From.Save (Outcome);
   end Finish;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Application)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Enter in the application setup
   --  ------------------------------
   procedure Setup (App    : in out Application;
                    Config : in String;
                    Server : in out ASF.Server.Container'Class) is
      Path : constant String := AWA.Applications.Configs.Get_Config_Path (Config);
      Dir  : constant String := Ada.Directories.Containing_Directory (Path);
      Done : constant String := Ada.Directories.Compose (Dir, ".initialized");
   begin
      Log.Info ("Entering configuration for {0}", Path);
      App.Name := To_Unbounded_String (Config);
      App.Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
      begin
         App.Config.Load_Properties (Path);
         --  Util.Log.Loggers.Initialize (Path);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Cannot read application configuration file: {0}", Path);
      end;

      --  If the Done file marker exists, the installation was done
      --  and we don't want to enter in it again.
      if Ada.Directories.Exists (Done) then
         Log.Info ("Application {0} is already initialized.", Config);
         Log.Info ("Remove {0} if you want to enter in the installation again.",
                   Done);
         return;
      end if;
      App.Initialize (App.Config, App.Factory);
      App.Set_Error_Page (ASF.Responses.SC_NOT_FOUND, "/setup/install.html");
      App.Set_Global ("contextPath", App.Config.Get ("contextPath"));
      App.Set_Global ("setup",
                      Util.Beans.Objects.To_Object (App'Unchecked_Access,
                        Util.Beans.Objects.STATIC));
      App.Add_Servlet (Name   => "redirect",
                       Server => App.Redirect'Unchecked_Access);
      App.Add_Servlet (Name   => "faces",
                       Server => App.Faces'Unchecked_Access);
      App.Add_Servlet (Name   => "files",
                       Server => App.Files'Unchecked_Access);
      App.Add_Mapping (Pattern => "*.html",
                       Name    => "redirect");
      App.Add_Mapping (Pattern => "/setup/*.html",
                       Name    => "faces");
      App.Add_Mapping (Pattern => "*.css",
                       Name    => "files");
      App.Add_Mapping (Pattern => "*.js",
                       Name    => "files");
      App.Add_Mapping (Pattern => "*.png",
                       Name    => "files");
      App.Add_Components (AWA.Components.Factory.Definition);

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
         Pos : constant Natural := Util.Strings.Index (URL, '#');
      begin
         if Pos > 0 then
            App.Changed.Set ("callback_url", URL (URL'First .. Pos - 1));
         else
            App.Changed.Set ("callback_url", "http://mydomain.com/oauth");
         end if;
      end;
      App.Database.Set_Connection (App.Get_Config (AWA.Applications.P_Database.P));
      App.Driver := Util.Beans.Objects.To_Object (App.Database.Get_Driver);
      if App.Database.Get_Driver = "mysql" then
         App.Db_Host := Util.Beans.Objects.To_Object (App.Database.Get_Server);
         App.Db_Port := Util.Beans.Objects.To_Object (App.Database.Get_Port);
      else
         App.Db_Host := Util.Beans.Objects.To_Object (String '("localhost"));
         App.Db_Port := Util.Beans.Objects.To_Object (Integer (3306));
      end if;
      Server.Register_Application (App.Get_Config (AWA.Applications.P_Context_Path.P),
                                   App'Unchecked_Access);
      Log.Info ("Connect your browser to {0}/index.html", App.Get_Config (P_Base_URL.P));
      App.Status.Wait_Configuring;

      Log.Info ("Application setup is now finished");
      Log.Info ("Creating the installation marker file {0}", Done);
      Util.Files.Write_File (Done, "installed");
   end Setup;

   --  ------------------------------
   --  Configure the application by using the setup application, allowing
   --  the administrator to setup the application database, define the application
   --  admin parameters.  After the configuration is done, register the
   --  application in the server container and start it.
   --  ------------------------------
   procedure Configure (Server : in out ASF.Server.Container'Class;
                        App    : in Application_Access;
                        Config : in String;
                        URI    : in String) is
      Path : constant String := AWA.Applications.Configs.Get_Config_Path (Config);
      S    : aliased Application;
      C    : ASF.Applications.Config;
   begin
      --  Do the application setup.
      S.Setup (Config, Server);

      --  Load the application configuration file that was configured
      --  during the setup process.
      begin
         C.Load_Properties (Path);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Cannot read application configuration file {0}", Path);
      end;

      --  Initialize the application and register it.
      Log.Info ("Initializing application {0}", URI);
      Initialize (App, C);
      Server.Register_Application (URI, App.all'Unchecked_Access);
      S.Status.Set (READY);
      delay 2.0;

      --  Now we can remove the setup application.
      Server.Remove_Application (S'Unchecked_Access);
   end Configure;

end AWA.Setup.Applications;
