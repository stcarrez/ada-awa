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
with EL.Contexts.Default;

--  The <b>AWA.Modules.Reader</b> package reads the module configuration files
--  and initializes the module.
package AWA.Modules.Reader is

   --  Read the module configuration file and configure the components
   procedure Read_Configuration (Plugin  : in out Module'Class;
                                 File    : in String;
                                 Context : in EL.Contexts.Default.Default_Context_Access);

end AWA.Modules.Reader;
