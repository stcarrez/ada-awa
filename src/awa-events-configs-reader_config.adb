-----------------------------------------------------------------------
--  awa-events-configs -- Event configuration
--  Copyright (C) 2012, 2013, 2017, 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Services.Contexts;
package body AWA.Events.Configs.Reader_Config is

   procedure Initialize is
   begin
      Add_Mapping (Mapper, Config'Unchecked_Access);
      Config.Manager     := Manager;
      Config.Context     := Context;
      Config.Session     := AWA.Services.Contexts.Get_Session (AWA.Services.Contexts.Current);
   end Initialize;

end AWA.Events.Configs.Reader_Config;
