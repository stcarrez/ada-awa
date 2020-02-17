-----------------------------------------------------------------------
--  awa-applications-configs -- Read application configuration files
--  Copyright (C) 2011, 2012, 2015, 2017, 2020 Stephane Carrez
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
with EL.Contexts.Default;
with Util.Serialize.Mappers;
with Keystore.Properties;

--  The <b>AWA.Applications.Configs</b> package reads the application configuration files.
package AWA.Applications.Configs is

   MAX_PREFIX_LENGTH : constant := 64;

   --  Merge the configuration content and the keystore to a final configuration object.
   --  The keystore can be used to store sensitive information such as database connection,
   --  secret keys while the rest of the configuration remains in clear property files.
   --  The keystore must be unlocked to have access to its content.
   --  The prefix parameter is used to prefix names from the keystore so that the same
   --  keystore could be used by several applications.
   procedure Merge (Into   : in out ASF.Applications.Config;
                    Config : in out ASF.Applications.Config;
                    Wallet : in out Keystore.Properties.Manager;
                    Prefix : in String) with Pre => Prefix'Length <= MAX_PREFIX_LENGTH;

   --  XML reader configuration.  By instantiating this generic package, the XML parser
   --  gets initialized to read the configuration for servlets, filters, managed beans,
   --  permissions, events and other configuration elements.
   generic
      Mapper  : in out Util.Serialize.Mappers.Processing;
      App     : in Application_Access;
      Context : in EL.Contexts.Default.Default_Context_Access;
      Override_Context : in Boolean;
   package Reader_Config is
   end Reader_Config;

   --  Read the application configuration file and configure the application
   procedure Read_Configuration (App     : in out Application'Class;
                                 File    : in String;
                                 Context : in EL.Contexts.Default.Default_Context_Access;
                                 Override_Context : in Boolean);

   --  Get the configuration path for the application name.
   --  The configuration path is search from:
   --  o the current directory,
   --  o the 'config' directory,
   --  o the Dynamo installation directory in share/dynamo
   function Get_Config_Path (Name : in String) return String;

end AWA.Applications.Configs;
