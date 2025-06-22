-----------------------------------------------------------------------
--  awa-applications-configs -- Read application configuration files
--  Copyright (C) 2011, 2012, 2015, 2017, 2018, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;

with Util.Strings;
with Util.Properties;
with Util.Beans.Objects;
with Util.Files;
with Util.Log.Loggers;
with Util.Serialize.IO.XML;

with ASF.Contexts.Faces;
with ASF.Applications.Main.Configs;

with Security.Policies;
with AWA.Events.Configs.Reader_Config;
with AWA.Services.Contexts;

package body AWA.Applications.Configs is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Applications.Configs");

   type Wallet_Manager is limited new Util.Properties.Implementation.Manager with record
      Wallet : Keystore.Properties.Manager;
      Props  : ASF.Applications.Config;
      Length : Positive;
      Prefix : String (1 .. MAX_PREFIX_LENGTH);
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Wallet_Manager;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   overriding
   procedure Set_Value (From  : in out Wallet_Manager;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Returns TRUE if the property exists.
   overriding
   function Exists (Self : in Wallet_Manager;
                    Name : in String)
                    return Boolean;

   --  Remove the property given its name.
   overriding
   procedure Remove (Self : in out Wallet_Manager;
                     Name : in String);

   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   overriding
   procedure Iterate (Self    : in Wallet_Manager;
                      Process : access procedure (Name : in String;
                                                  Item : in Util.Beans.Objects.Object));

   --  Deep copy of properties stored in 'From' to 'To'.
   overriding
   function Create_Copy (Self : in Wallet_Manager)
                         return Util.Properties.Implementation.Manager_Access;

   package Shared_Manager is
     new Util.Properties.Implementation.Shared_Implementation (Wallet_Manager);

   subtype Property_Map is Shared_Manager.Manager;
   type Property_Map_Access is access all Property_Map;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Wallet_Manager;
                       Name : in String) return Util.Beans.Objects.Object is
      Prefixed_Name : constant String := From.Prefix (1 .. From.Length) & Name;
   begin
      if From.Wallet.Exists (Prefixed_Name) then
         declare
            Value : constant String := From.Wallet.Get (Prefixed_Name);
         begin
            return Util.Beans.Objects.To_Object (Value);
         end;
      else
         return From.Props.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the map contains the given name, the value changed.
   --  Otherwise name is added to the map and the value associated with it.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Wallet_Manager;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
      Prefixed_Name : constant String := From.Prefix (1 .. From.Length) & Name;
   begin
      if From.Wallet.Exists (Prefixed_Name) then
         From.Wallet.Set_Value (Prefixed_Name, Value);
      else
         From.Props.Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Returns TRUE if the property exists.
   --  ------------------------------
   overriding
   function Exists (Self : in Wallet_Manager;
                    Name : in String)
                    return Boolean is
   begin
      return Self.Props.Exists (Name)
        or else Self.Wallet.Exists (Self.Prefix (1 .. Self.Length) & Name);
   end Exists;

   --  ------------------------------
   --  Remove the property given its name.
   --  ------------------------------
   overriding
   procedure Remove (Self : in out Wallet_Manager;
                     Name : in String) is
      Prefixed_Name : constant String := Self.Prefix (1 .. Self.Length) & Name;
   begin
      if Self.Wallet.Exists (Prefixed_Name) then
         Self.Wallet.Remove (Prefixed_Name);
      else
         Self.Props.Remove (Name);
      end if;
   end Remove;

   --  ------------------------------
   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   --  ------------------------------
   overriding
   procedure Iterate (Self    : in Wallet_Manager;
                      Process : access procedure (Name : in String;
                                                  Item : in Util.Beans.Objects.Object)) is
      procedure Wallet_Filter (Name : in String;
                               Item : in Util.Beans.Objects.Object);
      procedure Property_Filter (Name : in String;
                               Item : in Util.Beans.Objects.Object);

      procedure Wallet_Filter (Name : in String;
                               Item : in Util.Beans.Objects.Object) is
      begin
         if Util.Strings.Starts_With (Name, Self.Prefix (1 .. Self.Length)) then
            Log.Debug ("Use wallet property {0} as {1}",
                       Name, Name (Name'First + Self.Length .. Name'Last));
            Process (Name (Name'First + Self.Length .. Name'Last), Item);
         end if;
      end Wallet_Filter;

      procedure Property_Filter (Name : in String;
                               Item : in Util.Beans.Objects.Object) is
         Prefixed_Name : constant String := Self.Prefix (1 .. Self.Length) & Name;
      begin
         if not Self.Wallet.Exists (Prefixed_Name) then
            Process (Name, Item);
         end if;
      end Property_Filter;

   begin
      Self.Props.Iterate (Property_Filter'Access);
      Self.Wallet.Iterate (Wallet_Filter'Access);
   end Iterate;

   --  ------------------------------
   --  Deep copy of properties stored in 'From' to 'To'.
   --  ------------------------------
   overriding
   function Create_Copy (Self : in Wallet_Manager)
                         return Util.Properties.Implementation.Manager_Access is
      Result : constant Property_Map_Access := new Property_Map;
   begin
      Result.Length := Self.Length;
      Result.Wallet := Self.Wallet;
      Result.Props := Self.Props;
      Result.Prefix := Self.Prefix;
      return Result.all'Access;
   end Create_Copy;

   --  ------------------------------
   --  Merge the configuration content and the keystore to a final configuration object.
   --  The keystore can be used to store sensitive information such as database connection,
   --  secret keys while the rest of the configuration remains in clear property files.
   --  The keystore must be unlocked to have access to its content.
   --  The prefix parameter is used to prefix names from the keystore so that the same
   --  keystore could be used by several applications.
   --  ------------------------------
   procedure Merge (Into   : in out ASF.Applications.Config;
                    Config : in out ASF.Applications.Config;
                    Wallet : in out Keystore.Properties.Manager;
                    Prefix : in String) is
      function Allocate return Util.Properties.Implementation.Shared_Manager_Access;

      function Allocate return Util.Properties.Implementation.Shared_Manager_Access is
         Result : constant Property_Map_Access := new Property_Map;
      begin
         Result.Length := Prefix'Length;
         Result.Wallet := Wallet;
         Result.Props := Config;
         Result.Prefix (1 .. Result.Length) := Prefix;
         return Result.all'Access;
      end Allocate;

      procedure Setup is
        new Util.Properties.Implementation.Initialize (Allocate);

   begin
      Setup (Into);
   end Merge;

   --  ------------------------------
   --  XML reader configuration.  By instantiating this generic package, the XML parser
   --  gets initialized to read the configuration for servlets, filters, managed beans,
   --  permissions, events and other configuration elements.
   --  ------------------------------
   package body Reader_Config is
      App_Access : constant ASF.Contexts.Faces.Application_Access := App.all'Unchecked_Access;

      package Bean_Config is
        new ASF.Applications.Main.Configs.Reader_Config (Mapper, App_Access,
                                                         Context.all'Access,
                                                         Override_Context);

      package Event_Config is
        new AWA.Events.Configs.Reader_Config (Mapper  => Mapper,
                                              Manager => App.Events'Unchecked_Access,
                                              Context => Context.all'Access);

      pragma Warnings (Off, Bean_Config);
      pragma Warnings (Off, Event_Config);

   begin
      Event_Config.Initialize;
   end Reader_Config;

   --  ------------------------------
   --  Read the application configuration file and configure the application
   --  ------------------------------
   procedure Read_Configuration (App     : in out Application'Class;
                                 File    : in String;
                                 Context : in EL.Contexts.Default.Default_Context_Access;
                                 Override_Context : in Boolean) is

      Reader : Util.Serialize.IO.XML.Parser;
      Mapper : Util.Serialize.Mappers.Processing;
      Ctx    : AWA.Services.Contexts.Service_Context;
      Sec    : constant Security.Policies.Policy_Manager_Access := App.Get_Security_Manager;
   begin
      Log.Info ("Reading application configuration file {0}", File);
      Ctx.Set_Context (App'Unchecked_Access, null);

      declare
         package Config is new Reader_Config (Mapper, App'Unchecked_Access, Context,
                                              Override_Context);
         pragma Warnings (Off, Config);
      begin
         Sec.Prepare_Config (Mapper);

         --  Initialize the parser with the module configuration mappers (if any).
         Initialize_Parser (App, Reader);

         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Util.Serialize.Mappers.Dump (Mapper, Log);
         end if;

         --  Read the configuration file and record managed beans, navigation rules.
         Reader.Parse (File, Mapper);
      end;
   exception
      when others =>
         Log.Error ("Error while reading {0}", File);
         raise;
   end Read_Configuration;

   --  ------------------------------
   --  Get the configuration path for the application name.
   --  The configuration path is search from:
   --  o the current directory,
   --  o the 'config' directory,
   --  o the Dynamo installation directory in share/dynamo
   --  ------------------------------
   function Get_Config_Path (Name : in String) return String is
      use Ada.Strings.Unbounded;
      use Ada.Directories;

      Command : constant String := Ada.Command_Line.Command_Name;
      Path    : constant String := Ada.Directories.Containing_Directory (Command);
      Dir     : constant String := Containing_Directory (Path);
      Paths   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Paths, ".;");
      Append (Paths, Util.Files.Compose (Dir, "config"));
      Append (Paths, ";");
      Append (Paths, Util.Files.Compose (Dir, "share/dynamo/" & Name));
      return Util.Files.Find_File_Path (Name & ".properties", To_String (Paths));
   end Get_Config_Path;

end AWA.Applications.Configs;
