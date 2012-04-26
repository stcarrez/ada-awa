-----------------------------------------------------------------------
--  awa-modules-reader -- Read module configuration files
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with Util.Serialize.IO.XML;

with ASF.Applications.Main.Configs;

with AWA.Applications.Configs;
with AWA.Services.Contexts;

--  The <b>AWA.Modules.Reader</b> package reads the module configuration files
--  and initializes the module.
package body AWA.Modules.Reader is

   --  ------------------------------
   --  Read the module configuration file and configure the components
   --  ------------------------------
   procedure Read_Configuration (Plugin  : in out Module'Class;
                                 File    : in String;
                                 Context : in EL.Contexts.Default.Default_Context_Access) is

      Reader : Util.Serialize.IO.XML.Parser;
      Ctx    : AWA.Services.Contexts.Service_Context;

      package Config is
        new AWA.Applications.Configs.Reader_Config (Reader,
                                                    Plugin.App.all'Unchecked_Access,
                                                    Context);
      pragma Warnings (Off, Config);

   begin
      Log.Info ("Reading module configuration file {0}", File);

      Ctx.Set_Context (Plugin.App.all'Unchecked_Access, null);

      if AWA.Modules.Log.Get_Level >= Util.Log.DEBUG_LEVEL then
         Util.Serialize.IO.Dump (Reader, AWA.Modules.Log);
      end if;

      --  Read the configuration file and record managed beans, navigation rules.
      Reader.Parse (File);

   exception
      when others =>
         Log.Error ("Error while reading {0}", File);
         raise;
   end Read_Configuration;

end AWA.Modules.Reader;
