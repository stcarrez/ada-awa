-----------------------------------------------------------------------
--  awa-modules-reader -- Read module configuration files
--  Copyright (C) 2011, 2012, 2015, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
