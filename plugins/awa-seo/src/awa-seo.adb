-----------------------------------------------------------------------
--  awa-seo -- Search Engine Optimization module
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Log.Loggers;

with AWA.SEO.Modules;
package body AWA.SEO is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.SEO");

   procedure Register (Name     : in String;
                       Provider : Sitemap_Provider_Access) is
      use type AWA.SEO.Modules.SEO_Module_Access;

      SEO_Module : constant AWA.SEO.Modules.SEO_Module_Access
        := AWA.SEO.Modules.Get_SEO_Module;
   begin
      if SEO_Module /= null then
         Log.Info ("Registered sitemap {0}", Name);
         SEO_Module.Register (Name, Provider);
      else
         Log.Warn ("No SEO module to register sitmap {0}", Name);
      end if;
   end Register;

end AWA.SEO;
