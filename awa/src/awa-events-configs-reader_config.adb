-----------------------------------------------------------------------
--  awa-events-configs -- Event configuration
--  Copyright (C) 2012, 2013, 2017, 2018, 2020 Stephane Carrez
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
