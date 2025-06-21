-----------------------------------------------------------------------
--  awa-modules-get -- Get a specific module instance
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>Get</b> function is an helper that retrieves a given module
--  instance from the current application.
generic
   type Module_Type is new Module with private;
   type Module_Type_Access is access all Module_Type'Class;
   Name : String;
function AWA.Modules.Get return Module_Type_Access;
