-----------------------------------------------------------------------
--  awa-commands -- AWA commands for server or admin tool
--  Copyright (C) 2019, 2020, 2021, 2022 Stephane Carrez
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
with Ada.IO_Exceptions;
with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Util.Log.Loggers;
with Util.Strings.Tokenizers;
with Util.Strings.Vectors;
with AWA.Applications.Configs;
with Keystore.Passwords.Input;
with Keystore.Passwords.Files;
with Keystore.Passwords.Unsafe;
with Keystore.Passwords.Cmds;
package body AWA.Commands is

   use Ada.Strings.Unbounded;
   use type Keystore.Passwords.Provider_Access;
   use type Keystore.Header_Slot_Count_Type;
   use type Keystore.Passwords.Keys.Key_Provider_Access;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Commands");

   procedure Load_Configuration (Context : in out Context_Type;
                                 Path    : in String) is
   begin
      Log.Info ("Loading server configuration {0}", Path);
      begin
         Context.Global_Config.Load_Properties (Path);
         Configure_Logs (Util.Properties.Manager (Context.Global_Config),
                         Root => Context.Global_Config.Get ("log4j.rootCategory", ""),
                         Debug => Context.Debug,
                         Dump => Context.Dump,
                         Verbose => Context.Verbose);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Cannot read server configuration file '{0}'",
                       Path);
      end;
      if Context.Exists (GPG_CRYPT_CONFIG) then
         Context.GPG.Set_Encrypt_Command (Context.Get_Config (GPG_CRYPT_CONFIG));
      end if;
      if Context.Exists (GPG_DECRYPT_CONFIG) then
         Context.GPG.Set_Decrypt_Command (Context.Get_Config (GPG_DECRYPT_CONFIG));
      end if;
      if Context.Exists (GPG_LIST_CONFIG) then
         Context.GPG.Set_List_Key_Command (Context.Get_Config (GPG_LIST_CONFIG));
      end if;
      Context.Config.Randomize := not Context.Zero;
   end Load_Configuration;

   --  ------------------------------
   --  Returns True if a keystore is used by the configuration and must be unlocked.
   --  ------------------------------
   function Use_Keystore (Context : in Context_Type) return Boolean is
   begin
      if Context.Wallet_File'Length > 0 then
         return True;
      else
         return Context.Exists (KEYSTORE_PATH);
      end if;
   end Use_Keystore;

   --  ------------------------------
   --  Open the keystore file using the password password.
   --  ------------------------------
   procedure Open_Keystore (Context    : in out Context_Type) is
   begin
      Setup_Password_Provider (Context);
      Setup_Key_Provider (Context);

      Context.Wallet.Open (Path      => Context.Get_Keystore_Path,
                           Data_Path => "",
                           Config    => Context.Config,
                           Info      => Context.Info);

      if not Context.No_Password_Opt or else Context.Info.Header_Count = 0 then
         if Context.Key_Provider /= null then
            Context.Wallet.Set_Master_Key (Context.Key_Provider.all);
         end if;
         if Context.Provider = null then
            Context.Provider := Keystore.Passwords.Input.Create (-("Enter password: "), False);
         end if;

         Context.Wallet.Unlock (Context.Provider.all, Context.Slot);
      else
         Context.GPG.Load_Secrets (Context.Wallet);

         Context.Wallet.Set_Master_Key (Context.GPG);

         Context.Wallet.Unlock (Context.GPG, Context.Slot);
      end if;
      Context.Keystore_Opened := True;

      Keystore.Properties.Initialize (Context.Secure_Config,
                                      Context.Wallet'Unchecked_Access);
   end Open_Keystore;

   function Get_Application_Config (Context : in Context_Type;
                                    Name    : in String) return String is
   begin
      if Context.Exists (Name & ".config") then
         return Context.Get_Config (Name & ".config");
      else
         return AWA.Applications.Configs.Get_Config_Path (Name);
      end if;
   end Get_Application_Config;

   --  ------------------------------
   --  Configure the application by loading its configuration file and merging it with
   --  the keystore file if there is one.
   --  ------------------------------
   procedure Configure (Name        : in String;
                        Context     : in out Context_Type) is
      procedure Read_Configuration (Config_Path : in String;
                                    Done        : out Boolean);

      Path : constant String := Context.Get_Application_Config (Name);
      File_Config : ASF.Applications.Config;

      procedure Read_Configuration (Config_Path : in String;
                                    Done        : out Boolean) is
      begin
         Done := False;
         Log.Info ("Loading configuration '{0}'", Config_Path);
         File_Config.Load_Properties (Config_Path);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Configuration file {0} not found", Path);
            if Context.Exists (Name & ".config") then
               Log.Error ("Check configuration variable {0}.config", Name);
            elsif Context.Config_File'Length > 0 then
               Log.Error ("Add a configuration variable '{0}.config' in {1}",
                          Name, Context.Config_File.all);
            end if;
      end Read_Configuration;

   begin
      Log.Info ("Configuring {0} with {1}", Name, Path);

      --  The path may contain a list of configuration files to load.
      --  This allows to override some configuration.
      Util.Strings.Tokenizers.Iterate_Tokens (Content => Path,
                                              Pattern => ",",
                                              Process => Read_Configuration'Access);

      if Context.Use_Keystore then
         if not Context.Keystore_Opened then
            Open_Keystore (Context);
         end if;
         AWA.Applications.Configs.Merge (Context.App_Config,
                                         File_Config,
                                         Context.Secure_Config,
                                         Name & ".");
      else
         Context.App_Config := File_Config;
      end if;
   end Configure;

   --  ------------------------------
   --  Initialize the commands.
   --  ------------------------------
   overriding
   procedure Initialize (Context : in out Context_Type) is
   begin
      GC.Define_Switch (Config => Context.Command_Config,
                        Output => Context.Version'Access,
                        Switch => "-V",
                        Long_Switch => "--version",
                        Help   => -("Print the version"));
      GC.Define_Switch (Config => Context.Command_Config,
                        Output => Context.Verbose'Access,
                        Switch => "-v",
                        Long_Switch => "--verbose",
                        Help   => -("Verbose execution mode"));
      GC.Define_Switch (Config => Context.Command_Config,
                        Output => Context.Debug'Access,
                        Switch => "-vv",
                        Long_Switch => "--debug",
                        Help   => -("Enable debug execution"));
      GC.Define_Switch (Config => Context.Command_Config,
                        Output => Context.Dump'Access,
                        Switch => "-vvv",
                        Long_Switch => "--debug-dump",
                        Help   => -("Enable debug dump execution"));
      GC.Define_Switch (Config => Context.Command_Config,
                        Output => Context.Zero'Access,
                        Switch => "-z",
                        Long_Switch => "--zero",
                        Help   => -("Erase and fill with zeros instead of random values"));
      GC.Define_Switch (Config => Context.Command_Config,
                        Output => Context.Config_File'Access,
                        Switch => "-c:",
                        Long_Switch => "--config=",
                        Argument => "PATH",
                        Help   => -("Defines the path for configuration file"));
      GC.Initialize_Option_Scan (Stop_At_First_Non_Switch => True);
   end Initialize;

   --  ------------------------------
   --  Setup the command before parsing the arguments and executing it.
   --  ------------------------------
   procedure Setup_Command (Config  : in out GC.Command_Line_Configuration;
                            Context : in out Context_Type) is
   begin
      GC.Define_Switch (Config => Config,
                        Output => Context.Wallet_File'Access,
                        Switch => "-k:",
                        Long_Switch => "--keystore=",
                        Argument => "PATH",
                        Help   => -("Defines the path for the keystore file"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Password_File'Access,
                        Long_Switch => "--passfile=",
                        Argument => "PATH",
                        Help   => -("Read the file that contains the password"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Unsafe_Password'Access,
                        Long_Switch => "--passfd=",
                        Argument => "NUM",
                        Help   => -("Read the password from the pipe with"
                          & " the given file descriptor"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Unsafe_Password'Access,
                        Long_Switch => "--passsocket=",
                        Help   => -("The password is passed within the socket connection"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Password_Env'Access,
                        Long_Switch => "--passenv=",
                        Argument => "NAME",
                        Help   => -("Read the environment variable that contains"
                        & " the password (not safe)"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Unsafe_Password'Access,
                        Switch => "-p:",
                        Long_Switch => "--password=",
                        Help   => -("The password is passed within the command line (not safe)"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Password_Askpass'Access,
                        Long_Switch => "--passask",
                        Help   => -("Run the ssh-askpass command to get the password"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Password_Command'Access,
                        Long_Switch => "--passcmd=",
                        Argument => "COMMAND",
                        Help   => -("Run the command to get the password"));
      GC.Define_Switch (Config => Config,
                        Output => Context.Wallet_Key_File'Access,
                        Long_Switch => "--wallet-key-file=",
                        Argument => "PATH",
                        Help   => -("Read the file that contains the wallet keys"));
   end Setup_Command;

   procedure Setup_Password_Provider (Context : in out Context_Type) is
   begin
      if Context.Password_Askpass then
         Context.Provider := Keystore.Passwords.Cmds.Create ("ssh-askpass");
      elsif Context.Password_Command'Length > 0 then
         Context.Provider := Keystore.Passwords.Cmds.Create (Context.Password_Command.all);
      elsif Context.Password_File'Length > 0 then
         Context.Provider := Keystore.Passwords.Files.Create (Context.Password_File.all);
      elsif Context.Password_Command'Length > 0 then
         Context.Provider := Keystore.Passwords.Cmds.Create (Context.Password_Command.all);
      elsif Context.Unsafe_Password'Length > 0 then
         Context.Provider := Keystore.Passwords.Unsafe.Create (Context.Unsafe_Password.all);
      elsif Context.Exists (PASSWORD_FILE_PATH) then
         Context.Provider := Keystore.Passwords.Files.Create
           (Context.Get_Config (PASSWORD_FILE_PATH));
      else
         Context.No_Password_Opt := True;
      end if;
   end Setup_Password_Provider;

   procedure Setup_Key_Provider (Context : in out Context_Type) is
   begin
      if Context.Wallet_Key_File'Length > 0 then
         Context.Key_Provider := Keystore.Passwords.Files.Create
           (Context.Wallet_Key_File.all);
      elsif Context.Exists (WALLET_KEY_PATH) then
         Context.Key_Provider := Keystore.Passwords.Files.Create
           (Context.Get_Config (WALLET_KEY_PATH));
      else
         Context.Key_Provider := Keystore.Passwords.Keys.Create
           (Keystore.DEFAULT_WALLET_KEY);
      end if;
   end Setup_Key_Provider;

   --  ------------------------------
   --  Get the keystore file path.
   --  ------------------------------
   function Get_Keystore_Path (Context : in out Context_Type) return String is
   begin
      if Context.Wallet_File'Length > 0 then
         Context.First_Arg := 1;
         return Context.Wallet_File.all;
      elsif Context.Exists (KEYSTORE_PATH) then
         return Context.Get_Config (KEYSTORE_PATH);
      else
         raise Error with "No keystore path";
      end if;
   end Get_Keystore_Path;

   procedure Print (Context : in out Context_Type;
                    Ex      : in Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (Context);
   begin
      Ada.Exceptions.Reraise_Occurrence (Ex);

   exception
      when GNAT.Command_Line.Exit_From_Command_Line | GNAT.Command_Line.Invalid_Switch =>
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when Keystore.Bad_Password =>
         Log.Error (-("Invalid password to unlock the keystore file"));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when Keystore.No_Key_Slot =>
         Log.Error (-("There is no available key slot to add the password"));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when Keystore.No_Content =>
         Log.Error (-("No content for an item of type wallet"));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when Keystore.Corrupted =>
         Log.Error (-("The keystore file is corrupted: invalid meta data content"));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when Keystore.Invalid_Block =>
         Log.Error (-("The keystore file is corrupted: invalid data block headers or signature"));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when Keystore.Invalid_Signature =>
         Log.Error (-("The keystore file is corrupted: invalid signature"));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when Keystore.Invalid_Storage =>
         Log.Error (-("The keystore file is corrupted: invalid or missing storage file"));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when Keystore.Invalid_Keystore =>
         Log.Error (-("The file is not a keystore"));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when AWA.Commands.Error | Util.Commands.Not_Found =>
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when E : AWA.Applications.Start_Error =>
         Log.Error (-("Cannot start the server: {0}"), Ada.Exceptions.Exception_Message (E));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when E : Ada.IO_Exceptions.Name_Error =>
         Log.Error (-("Cannot access file: {0}"), Ada.Exceptions.Exception_Message (E));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      when E : others =>
         Log.Error (-("Some internal error occurred"), E);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   end Print;

   --  ------------------------------
   --  Configure the logs.
   --  ------------------------------
   procedure Configure_Logs (Log_Config  : in out Util.Properties.Manager;
                             Root        : in String;
                             Debug       : in Boolean;
                             Dump        : in Boolean;
                             Verbose     : in Boolean) is
      procedure Split (Item : in String;
                       Done : out Boolean);
      function Make_Root (Level     : in String;
                          Appender1 : in String;
                          Appender2 : in String) return String;

      Start : constant Natural := Util.Strings.Index (Root, ',');
      List  : Util.Strings.Vectors.Vector;

      procedure Split (Item : in String;
                       Done : out Boolean) is
      begin
         Done := False;
         List.Append (Item);
      end Split;

      function Make_Root (Level     : in String;
                          Appender1 : in String;
                          Appender2 : in String) return String is
         Result : Unbounded_String;
      begin
         Append (Result, Level);
         if not List.Contains (Appender1) then
            Append (Result, Appender1);
         end if;
         if Appender2'Length > 0 and then not List.Contains (Appender2) then
            Append (Result, ",");
            Append (Result, Appender2);
         end if;
         for Item of List loop
            Append (Result, ",");
            Append (Result, Item);
         end loop;
         return To_String (Result);
      end Make_Root;

   begin
      if Start > 0 then
         Util.Strings.Tokenizers.Iterate_Tokens (Root (Start + 1 .. Root'Last),
                                                 ",", Split'Access);
      end if;

      Log_Config.Set ("log4j.rootCategory", Make_Root ("DEBUG", "console", ""));
      Log_Config.Set ("log4j.appender.console", "Console");
      Log_Config.Set ("log4j.appender.console.level", "ERROR");
      Log_Config.Set ("log4j.appender.console.layout", "message");
      Log_Config.Set ("log4j.appender.console.stderr", "true");
      Log_Config.Set ("log4j.logger.Util", "FATAL");
      Log_Config.Set ("log4j.logger.log", "ERROR");
      if Verbose or Debug or Dump then
         Log_Config.Set ("log4j.logger.log", "INFO");
         Log_Config.Set ("log4j.logger.Util", "WARN");
         Log_Config.Set ("log4j.logger.AWA", "INFO");
         Log_Config.Set ("log4j.logger.AWA.Commands", "INFO");
         Log_Config.Set ("log4j.logger.Keystore.IO", "WARN");
         Log_Config.Set ("log4j.logger.ADO.Sessions", "WARN");
         Log_Config.Set ("log4j.rootCategory", Make_Root ("DEBUG", "console", "verbose"));
         Log_Config.Set ("log4j.appender.verbose", "Console");
         Log_Config.Set ("log4j.appender.verbose.level", "INFO");
         Log_Config.Set ("log4j.appender.verbose.layout", "level-message");
      end if;
      if Debug or Dump then
         Log_Config.Set ("log4j.logger.log", "INFO");
         Log_Config.Set ("log4j.logger.Util.Processes", "INFO");
         Log_Config.Set ("log4j.logger.Keystore.IO", "INFO");
         Log_Config.Set ("log4j.logger.ADO", "INFO");
         Log_Config.Set ("log4j.rootCategory", Make_Root ("DEBUG", "console", "debug"));
         Log_Config.Set ("log4j.appender.debug", "Console");
         Log_Config.Set ("log4j.appender.debug.level", "DEBUG");
         Log_Config.Set ("log4j.appender.debug.layout", "full");
      end if;
      if Dump then
         Log_Config.Set ("log4j.logger.Keystore.IO", "DEBUG");
         Log_Config.Set ("log4j.logger.AWA", "DEBUG");
         Log_Config.Set ("log4j.logger.ADO", "DEBUG");
         Log_Config.Set ("log4j.logger.ADO.Sessions", "WARN");
      end if;

      Util.Log.Loggers.Initialize (Log_Config);

   end Configure_Logs;

   procedure Configure_Logs (Root        : in String;
                             Debug       : in Boolean;
                             Dump        : in Boolean;
                             Verbose     : in Boolean) is
      Log_Config : Util.Properties.Manager;
   begin
      Configure_Logs (Log_Config, Root, Debug, Dump, Verbose);
   end Configure_Logs;

   overriding
   procedure Finalize (Context : in out Context_Type) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Keystore.Passwords.Provider'Class,
                                        Name   => Keystore.Passwords.Provider_Access);
   begin
      GC.Free (Context.Command_Config);
      Free (Context.Provider);
   end Finalize;

end AWA.Commands;
