-----------------------------------------------------------------------
--  awa-modules-reader -- Read module configuration files
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
