-----------------------------------------------------------------------
--  awa-applications-configs -- Read application configuration files
--  Copyright (C) 2011, 2012, 2015 Stephane Carrez
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

with Util.Log.Loggers;

with ASF.Contexts.Faces;
with ASF.Applications.Main.Configs;

with Security.Policies;
with AWA.Events.Configs;
with AWA.Services.Contexts;

package body AWA.Applications.Configs is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Applications.Configs");

   --  ------------------------------
   --  XML reader configuration.  By instantiating this generic package, the XML parser
   --  gets initialized to read the configuration for servlets, filters, managed beans,
   --  permissions, events and other configuration elements.
   --  ------------------------------
   package body Reader_Config is
      App_Access : constant ASF.Contexts.Faces.Application_Access := App.all'Unchecked_Access;

      package Bean_Config is
        new ASF.Applications.Main.Configs.Reader_Config (Reader, App_Access,
                                                         Context.all'Access,
                                                         Override_Context);

      package Event_Config is
        new AWA.Events.Configs.Reader_Config (Reader  => Reader,
                                              Manager => App.Events'Unchecked_Access,
                                              Context => Context.all'Access);

      pragma Warnings (Off, Bean_Config);
--        pragma Warnings (Off, Policy_Config);
      pragma Warnings (Off, Event_Config);
   end Reader_Config;

   --  ------------------------------
   --  Read the application configuration file and configure the application
   --  ------------------------------
   procedure Read_Configuration (App     : in out Application'Class;
                                 File    : in String;
                                 Context : in EL.Contexts.Default.Default_Context_Access;
                                 Override_Context : in Boolean) is

      Reader : Util.Serialize.IO.XML.Parser;
      Ctx    : AWA.Services.Contexts.Service_Context;
      Sec    : constant Security.Policies.Policy_Manager_Access := App.Get_Security_Manager;
   begin
      Log.Info ("Reading application configuration file {0}", File);
      Ctx.Set_Context (App'Unchecked_Access, null);

      declare
         package Config is new Reader_Config (Reader, App'Unchecked_Access, Context,
                                              Override_Context);
         pragma Warnings (Off, Config);
      begin
         Sec.Prepare_Config (Reader);

         --  Initialize the parser with the module configuration mappers (if any).
         Initialize_Parser (App, Reader);

         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Util.Serialize.IO.Dump (Reader, Log);
         end if;

         --  Read the configuration file and record managed beans, navigation rules.
         Reader.Parse (File);
      end;
   exception
      when others =>
         Log.Error ("Error while reading {0}", File);
         raise;
   end Read_Configuration;

end AWA.Applications.Configs;
