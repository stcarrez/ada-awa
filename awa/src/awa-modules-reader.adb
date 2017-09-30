-----------------------------------------------------------------------
--  awa-modules-reader -- Read module configuration files
--  Copyright (C) 2011, 2012, 2015, 2017 Stephane Carrez
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
with Util.Serialize.Mappers;

with AWA.Applications.Configs;
with Security.Policies;

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
      Mapper : Util.Serialize.Mappers.Processing;

      package Config is
        new AWA.Applications.Configs.Reader_Config (Mapper,
                                                    Plugin.App.all'Unchecked_Access,
                                                    Context,
                                                    False);
      pragma Warnings (Off, Config);

      Sec    : constant Security.Policies.Policy_Manager_Access := Plugin.App.Get_Security_Manager;
   begin
      Log.Info ("Reading module configuration file {0}", File);

      Sec.Prepare_Config (Mapper);
      if AWA.Modules.Log.Get_Level >= Util.Log.DEBUG_LEVEL then
         Util.Serialize.Mappers.Dump (Mapper, AWA.Modules.Log);
      end if;

      --  Read the configuration file and record managed beans, navigation rules.
      Reader.Parse (File, Mapper);
      Sec.Finish_Config (Reader);

   exception
      when others =>
         Log.Error ("Error while reading {0}", File);
         raise;
   end Read_Configuration;

end AWA.Modules.Reader;
