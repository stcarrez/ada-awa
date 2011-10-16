-----------------------------------------------------------------------
--  awa-modules-reader -- Read module configuration files
--  Copyright (C) 2011 Stephane Carrez
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

with ASF.Navigations.Mappers;
with ASF.Servlets.Mappers;
with ASF.Beans.Mappers;

with Security.Permissions;
with Security.Controllers.Roles;
with AWA.Permissions.Configs;
with AWA.Services.Contexts;

--  The <b>AWA.Modules.Reader</b> package reads the module configuration files
--  and initializes the module.
package body AWA.Modules.Reader is

   --  ------------------------------
   --  Read the module configuration file and configure the components
   --  ------------------------------
   procedure Read_Configuration (Plugin  : in out Module'Class;
                                 File    : in String;
                                 Context : in EL.Contexts.ELContext_Access) is

      Reader     : Util.Serialize.IO.XML.Parser;

      Ctx : AWA.Services.Contexts.Service_Context;
      Nav : constant ASF.Navigations.Navigation_Handler_Access := Plugin.App.Get_Navigation_Handler;
      Sec : constant Security.Permissions.Permission_Manager_Access
        := Plugin.App.Get_Permission_Manager;

   begin
      Log.Info ("Reading module configuration file {0}", File);

      Ctx.Set_Context (Plugin.App.all'Unchecked_Access, null);
      declare
         package Bean_Config is
           new ASF.Beans.Mappers.Reader_Config (Reader, Plugin.Factory'Unchecked_Access, Context);
         package Navigation_Config is
           new ASF.Navigations.Mappers.Reader_Config (Reader, Nav, Context);
         package Servlet_Config is
           new ASF.Servlets.Mappers.Reader_Config (Reader, Plugin.App.all'Unchecked_Access);
         package Policy_Config is
           new Security.Permissions.Reader_Config (Reader, Sec);
         package Role_Config is
           new Security.Controllers.Roles.Reader_Config (Reader, Sec);
         package Entity_Config is
           new AWA.Permissions.Configs.Reader_Config (Reader, Sec);

         pragma Warnings (Off, Bean_Config);
         pragma Warnings (Off, Navigation_Config);
         pragma Warnings (Off, Servlet_Config);
         pragma Warnings (Off, Policy_Config);
         pragma Warnings (Off, Role_Config);
         pragma Warnings (Off, Entity_Config);
      begin
         if AWA.Modules.Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Util.Serialize.IO.Dump (Reader, AWA.Modules.Log);
         end if;

         --  Read the configuration file and record managed beans, navigation rules.
         Reader.Parse (File);

      exception
         when E : others =>
            Log.Error ("Error while reading {0}", File);
            raise;
      end;
   end Read_Configuration;

end AWA.Modules.Reader;
