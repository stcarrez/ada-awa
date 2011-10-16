-----------------------------------------------------------------------
--  awa-applications-configs -- Read application configuration files
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
with EL.Contexts;

--  The <b>AWA.Applications.Configs</b> package reads the application configuration files.
package AWA.Applications.Configs is

   --  Read the application configuration file and configure the application
   procedure Read_Configuration (App     : in out Application'Class;
                                 File    : in String;
                                 Context : in EL.Contexts.ELContext_Access);

end AWA.Applications.Configs;
