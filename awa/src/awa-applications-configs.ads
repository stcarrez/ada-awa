-----------------------------------------------------------------------
--  awa-applications-configs -- Read application configuration files
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
with EL.Contexts.Default;
with Util.Serialize.IO.XML;

--  The <b>AWA.Applications.Configs</b> package reads the application configuration files.
package AWA.Applications.Configs is

   --  XML reader configuration.  By instantiating this generic package, the XML parser
   --  gets initialized to read the configuration for servlets, filters, managed beans,
   --  permissions, events and other configuration elements.
   generic
      Reader  : in out Util.Serialize.IO.XML.Parser;
      App     : in Application_Access;
      Context : in EL.Contexts.Default.Default_Context_Access;
   package Reader_Config is
   end Reader_Config;

   --  Read the application configuration file and configure the application
   procedure Read_Configuration (App     : in out Application'Class;
                                 File    : in String;
                                 Context : in EL.Contexts.Default.Default_Context_Access);

end AWA.Applications.Configs;
