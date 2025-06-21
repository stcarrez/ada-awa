-----------------------------------------------------------------------
--  awa-modules -- Application Module
--  Copyright (C) 2009 - 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Requests;
with ASF.Responses;
with ASF.Server;
with Ada.IO_Exceptions;

with Util.Files;
with Util.Properties;

with EL.Contexts.Default;

with AWA.Modules.Reader;
with AWA.Services.Contexts;
with AWA.Applications;
package body AWA.Modules is

   package ASC renames AWA.Services.Contexts;

   --  ------------------------------
   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   --  ------------------------------
   function Get_Config (Plugin  : Module_Manager;
                        Name    : String;
                        Default : String := "") return String is
   begin
      return Plugin.Module.all.Get_Config (Name, Default);
   end Get_Config;

   --  ------------------------------
   --  Get the module configuration property identified by the `Config` parameter.
   --  If the property does not exist, the default configuration value is returned.
   --  ------------------------------
   function Get_Config (Plugin : in Module_Manager;
                        Config : in ASF.Applications.Config_Param) return String is
   begin
      return Plugin.Module.all.Get_Config (Config);
   end Get_Config;

   function Get_Config (Plugin : in Module_Manager;
                        Config : in ASF.Applications.Config_Param) return Integer is
   begin
      return Plugin.Module.all.Get_Config (Config);
   end Get_Config;

   function Get_Config (Plugin : in Module_Manager;
                        Config : in ASF.Applications.Config_Param) return Boolean is
   begin
      return Plugin.Module.all.Get_Config (Config);
   end Get_Config;

   --  ------------------------------
   --  Get the module name
   --  ------------------------------
   function Get_Name (Plugin : in Module) return String is
   begin
      return To_String (Plugin.Name);
   end Get_Name;

   --  ------------------------------
   --  Get the base URI for this module
   --  ------------------------------
   function Get_URI (Plugin : in Module) return String is
   begin
      return To_String (Plugin.URI);
   end Get_URI;

   --  ------------------------------
   --  Get the application in which this module is registered.
   --  ------------------------------
   function Get_Application (Plugin : in Module) return Application_Access is
   begin
      return Plugin.App;
   end Get_Application;

   --  ------------------------------
   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   --  ------------------------------
   function Get_Config (Plugin  : Module;
                        Name    : String;
                        Default : String := "") return String is
   begin
      return Plugin.Config.Get (Name, Default);
   end Get_Config;

   --  ------------------------------
   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   --  ------------------------------
   function Get_Config (Plugin  : in Module;
                        Name    : in String;
                        Default : in Integer := -1) return Integer is
      Value : constant String := Plugin.Config.Get (Name, Integer'Image (Default));
   begin
      return Integer'Value (Value);

   exception
      when Constraint_Error =>
         return Default;
   end Get_Config;

   --  ------------------------------
   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   --  ------------------------------
   function Get_Config (Plugin  : in Module;
                        Name    : in String;
                        Default : in Boolean := False) return Boolean is
      Value : constant String := Plugin.Config.Get (Name, Boolean'Image (Default));
   begin
      if Value in "yes" | "true" | "1" | "YES" | "TRUE" then
         return True;
      else
         return False;
      end if;

   exception
      when Constraint_Error =>
         return Default;
   end Get_Config;

   --  ------------------------------
   --  Get the module configuration property identified by the `Config` parameter.
   --  If the property does not exist, the default configuration value is returned.
   --  ------------------------------
   function Get_Config (Plugin : in Module;
                        Config : in ASF.Applications.Config_Param) return String is
   begin
      return Plugin.Config.Get (Config);
   end Get_Config;

   function Get_Config (Plugin : in Module;
                        Config : in ASF.Applications.Config_Param) return Integer is
   begin
      return Plugin.Config.Get (Config);
   end Get_Config;

   function Get_Config (Plugin : in Module;
                        Config : in ASF.Applications.Config_Param) return Boolean is
   begin
      return Plugin.Config.Get (Config);
   end Get_Config;

   --  ------------------------------
   --  Get the module configuration property identified by the `Config` parameter.
   --  If the property does not exist, the default configuration value is returned.
   --  ------------------------------
   function Get_Config (Plugin  : in Module;
                        Name    : in String;
                        Default : in String := "")
                        return EL.Expressions.Expression is

      type Event_ELResolver is new EL.Contexts.Default.Default_ELResolver with null record;

      overriding
      function Get_Value (Resolver : Event_ELResolver;
                          Context  : EL.Contexts.ELContext'Class;
                          Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                          Name     : Unbounded_String) return Util.Beans.Objects.Object;

      --  ------------------------------
      --  Get the value associated with a base object and a given property.
      --  ------------------------------
      overriding
      function Get_Value (Resolver : Event_ELResolver;
                          Context  : EL.Contexts.ELContext'Class;
                          Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                          Name     : Unbounded_String) return Util.Beans.Objects.Object is
      begin
         if Base /= null then
            return EL.Contexts.Default.Default_ELResolver (Resolver).Get_Value (Context, Base,
                                                                                Name);
         else
            return Util.Beans.Objects.To_Object (Plugin.Get_Config (To_String (Name), ""));
         end if;
      end Get_Value;

      Resolver : aliased Event_ELResolver;
      Context  : EL.Contexts.Default.Default_Context;
      Value    : constant String := Plugin.Get_Config (Name, Default);
   begin
      Context.Set_Resolver (Resolver'Unchecked_Access);
      return EL.Expressions.Reduce_Expression (EL.Expressions.Create_Expression (Value, Context),
                                               Context);

   exception
      when E : others =>
         Log.Error ("Invalid parameter ", E, True);
         return EL.Expressions.Create_Expression ("", Context);

   end Get_Config;

   --  ------------------------------
   --  Send the event to the module
   --  ------------------------------
   procedure Send_Event (Plugin  : in Module;
                         Content : in AWA.Events.Module_Event'Class) is
   begin
      Plugin.App.Send_Event (Content);
   end Send_Event;

   --  ------------------------------
   --  Find the module with the given name
   --  ------------------------------
   function Find_Module (Plugin : Module;
                         Name   : String) return Module_Access is
   begin
      if Plugin.Registry = null then
         return null;
      end if;
      return Find_By_Name (Plugin.Registry.all, Name);
   end Find_Module;

   --  ------------------------------
   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   --  ------------------------------
   procedure Register (Plugin : in out Module;
                       Name    : in String;
                       Bind    : in ASF.Beans.Class_Binding_Access) is
   begin
      Plugin.App.Register_Class (Name, Bind);
   end Register;

   --  ------------------------------
   --  Finalize the module.
   --  ------------------------------
   overriding
   procedure Finalize (Plugin : in out Module) is
   begin
      null;
   end Finalize;

   procedure Initialize (Manager : in out Module_Manager;
                         Module  : in AWA.Modules.Module'Class) is
   begin
      Manager.Module := Module.Self;
   end Initialize;

   overriding
   function Get_Value (Manager : in Module_Manager;
                       Name    : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Manager, Name);
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  Module manager
   --
   --  ------------------------------
   --  Get the database connection for reading
   --  ------------------------------
   function Get_Session (Manager : Module_Manager)
                            return ADO.Sessions.Session is
   begin
      return Manager.Module.Get_Session;
   end Get_Session;

   --  ------------------------------
   --  Get the database connection for writing
   --  ------------------------------
   function Get_Master_Session (Manager : Module_Manager)
                                   return ADO.Sessions.Master_Session is
   begin
      return Manager.Module.Get_Master_Session;
   end Get_Master_Session;

   --  ------------------------------
   --  Send the event to the module.  The module identified by `To` is
   --  found and the event is posted on its event channel.
   --  ------------------------------
   procedure Send_Event (Manager : in Module_Manager;
                         Content : in AWA.Events.Module_Event'Class) is
   begin
      Manager.Module.Send_Event (Content);
   end Send_Event;

   procedure Initialize (Plugin : in out Module;
                         App    : in Application_Access;
                         Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);
   begin
      Plugin.Self := Plugin'Unchecked_Access;
      Plugin.App := App;
   end Initialize;

   --  ------------------------------
   --  Initialize the registry
   --  ------------------------------
   procedure Initialize (Registry : in out Module_Registry;
                         Config   : in ASF.Applications.Config) is
   begin
      Registry.Config := Config;
   end Initialize;

   --  ------------------------------
   --  Register the module in the registry.
   --  ------------------------------
   procedure Register (Registry : in Module_Registry_Access;
                       App      : in Application_Access;
                       Plugin   : in Module_Access;
                       Name     : in String;
                       URI      : in String) is
      procedure Copy (Params : in Util.Properties.Manager'Class);

      procedure Copy (Params : in Util.Properties.Manager'Class) is
      begin
         Plugin.Config.Copy (From => Params, Prefix => Name & ".", Strip => True);
      end Copy;

      Paths : constant String := Registry.Config.Get (Applications.P_Module_Dir.P);
   begin
      Log.Info ("Register module '{0}' under URI '{1}'", Name, URI);

      if Plugin.Registry /= null then
         Log.Error ("Module '{0}' is already attached to a registry", Name);

         raise Program_Error with "Module '" & Name & "' already registered";
      end if;
      Plugin.App      := App;
      Plugin.Registry := Registry;
      Plugin.Name     := To_Unbounded_String (Name);
      Plugin.URI      := To_Unbounded_String (URI);
      Plugin.Registry.Name_Map.Insert (Name, Plugin);
      if URI /= "" then
         Plugin.Registry.URI_Map.Insert (URI, Plugin);
      end if;

      --  Load the module configuration file
      Log.Debug ("Module search path: {0}", Paths);
      declare
         Base  : constant String := Name & ".properties";
         Path  : constant String := Util.Files.Find_File_Path (Base, Paths);
      begin
         Plugin.Config.Load_Properties (Path => Path, Prefix => Name & ".", Strip => True);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Info ("Module configuration file '{0}' does not exist", Path);
      end;

      Plugin.Initialize (App, Plugin.Config);

      --  Read the module XML configuration file if there is one.
      declare
         Base : constant String := Plugin.Config.Get ("config", Name & ".xml");
         Path : constant String := Util.Files.Find_File_Path (Base, Paths);
         Ctx  : aliased EL.Contexts.Default.Default_Context;
      begin
         AWA.Modules.Reader.Read_Configuration (Plugin.all, Path, Ctx'Unchecked_Access);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Warn ("Module configuration file '{0}' does not exist", Path);
      end;

      --  Override the module configuration with the application configuration
      App.Get_Init_Parameters (Copy'Access);

      Plugin.Configure (Plugin.Config);

   exception
      when Constraint_Error =>
         Log.Error ("Another module is already registered "
                    & "under name '{0}' or URI '{1}'", Name, URI);
         raise;
   end Register;

   --  ------------------------------
   --  Find the module with the given name
   --  ------------------------------
   function Find_By_Name (Registry : Module_Registry;
                          Name     : String) return Module_Access is
      Pos : constant Module_Maps.Cursor := Module_Maps.Find (Registry.Name_Map, Name);
   begin
      if Module_Maps.Has_Element (Pos) then
         return Module_Maps.Element (Pos);
      end if;
      return null;
   end Find_By_Name;

   --  ------------------------------
   --  Find the module mapped to a given URI
   --  ------------------------------
   function Find_By_URI (Registry : Module_Registry;
                         URI      : String) return Module_Access is
      Pos : constant Module_Maps.Cursor := Module_Maps.Find (Registry.URI_Map, URI);
   begin
      if Module_Maps.Has_Element (Pos) then
         return Module_Maps.Element (Pos);
      end if;
      return null;
   end Find_By_URI;

   --  ------------------------------
   --  Iterate over the modules that have been registered and execute the `Process`
   --  procedure on each of the module instance.
   --  ------------------------------
   procedure Iterate (Registry : in Module_Registry;
                      Process  : access procedure (Plugin : in out Module'Class)) is
      Iter : Module_Maps.Cursor := Registry.Name_Map.First;
   begin
      while Module_Maps.Has_Element (Iter) loop
         Process (Module_Maps.Element (Iter).all);
         Module_Maps.Next (Iter);
      end loop;
   end Iterate;

   --  ------------------------------
   --  Get the database connection for reading
   --  ------------------------------
   function Get_Session (Manager : Module)
                         return ADO.Sessions.Session is
      pragma Unreferenced (Manager);

      Ctx : constant ASC.Service_Context_Access := ASC.Current;
   begin
      return ASC.Get_Session (Ctx);
   end Get_Session;

   --  ------------------------------
   --  Get the database connection for writing
   --  ------------------------------
   function Get_Master_Session (Manager : Module)
                                return ADO.Sessions.Master_Session is
      pragma Unreferenced (Manager);

      Ctx : constant ASC.Service_Context_Access := ASC.Current;
   begin
      return ASC.Get_Master_Session (Ctx);
   end Get_Master_Session;

   --  ------------------------------
   --  Add a listener to the module listener list.  The module will invoke the listener
   --  depending on events or actions that occur in the module.
   --  ------------------------------
   procedure Add_Listener (Into : in out Module;
                           Item : in Util.Listeners.Listener_Access) is
   begin
      Util.Listeners.Add_Listener (Into.Listeners, Item);
   end Add_Listener;

   --  ------------------------------
   --  Find the module with the given name in the application and add the listener to the
   --  module listener list.
   --  ------------------------------
   procedure Add_Listener (Plugin : in Module;
                           Name   : in String;
                           Item   : in Util.Listeners.Listener_Access) is
      M : constant Module_Access := Plugin.App.Find_Module (Name);
   begin
      if M = null then
         Log.Error ("Cannot find module {0} to add a lifecycle listener", Name);
      else
         M.Add_Listener (Item);
      end if;
   end Add_Listener;

   --  ------------------------------
   --  Remove a listener from the module listener list.
   --  ------------------------------
   procedure Remove_Listener (Into : in out Module;
                              Item : in Util.Listeners.Listener_Access) is
   begin
      Util.Listeners.Remove_Listener (Into.Listeners, Item);
   end Remove_Listener;

   --  Get per request manager => look in Request
   --  Get per session manager => look in Request.Get_Session
   --  Get per application manager => look in Application
   --  Get per pool manager => look in pool attached to Application
   function Get_Manager return Manager_Type_Access is

      procedure Process (Request  : in out ASF.Requests.Request'Class;
                         Response : in out ASF.Responses.Response'Class);

      Value : Util.Beans.Objects.Object;

      procedure Process (Request  : in out ASF.Requests.Request'Class;
                         Response : in out ASF.Responses.Response'Class) is
         pragma Unreferenced (Response);
      begin
         Value := Request.Get_Attribute (Name);
         if Util.Beans.Objects.Is_Null (Value) then
            declare
               M : constant Manager_Type_Access := new Manager_Type;
            begin
               Value := Util.Beans.Objects.To_Object (M.all'Unchecked_Access);
               Request.Set_Attribute (Name, Value);
            end;
         end if;
      end Process;

   begin
      ASF.Server.Update_Context (Process'Access);
      if Util.Beans.Objects.Is_Null (Value) then
         return null;
      end if;
      declare
         B : constant access Util.Beans.Basic.Readonly_Bean'Class
           := Util.Beans.Objects.To_Bean (Value);
      begin
         if not (B.all in Manager_Type'Class) then
            return null;
         end if;
         return Manager_Type'Class (B.all)'Unchecked_Access;
      end;
   end Get_Manager;

end AWA.Modules;
