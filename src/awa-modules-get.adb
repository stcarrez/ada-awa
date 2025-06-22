-----------------------------------------------------------------------
--  awa-modules-get -- Get a specific module instance
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Applications;

--  ------------------------------
--  The <b>Get</b> function is an helper that retrieves a given module
--  instance from the current application.
--  ------------------------------
function AWA.Modules.Get return Module_Type_Access is
   use type AWA.Applications.Application_Access;

   App : constant AWA.Applications.Application_Access := AWA.Applications.Current;
begin
   if App = null then
      return null;
   end if;

   declare
      M : constant Module_Access := App.Find_Module (Name);
   begin
      if M = null then
         return null;
      else
         return Module_Type (M.all)'Access;
      end if;
   end;
end AWA.Modules.Get;
