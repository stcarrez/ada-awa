-----------------------------------------------------------------------
--  awa-seo-modules -- Search Engine Optimization module
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Log.Loggers;

with AWA.Modules.Get;
with AWA.Applications;
package body AWA.SEO.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.SEO.Module");

   --  ------------------------------
   --  Initialize the seo module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out SEO_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the SEO module");

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
      App.Add_Servlet ("sitemaps", Plugin.SEO_Servlet'Unchecked_Access);
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out SEO_Module;
                        Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);

      Refresh : constant Integer := Plugin.Get_Config (PARAM_SITEMAP_REFRESH, 3600);
   begin
      if Refresh > 0 then
         Plugin.Data.Set_Refresh_Delay (Duration (Refresh));
      end if;
   end Configure;

   --  ------------------------------
   --  Get the SEO module.
   --  ------------------------------
   function Get_SEO_Module return SEO_Module_Access is
      function Get is new AWA.Modules.Get (SEO_Module, SEO_Module_Access, NAME);
   begin
      return Get;
   end Get_SEO_Module;

   --  Register the sitemap provider under the given name.
   --  The provider will be called for the generation of sitemap.
   procedure Register (Plugin   : in out SEO_Module;
                       Name     : in String;
                       Provider : Sitemap_Provider_Access) is
   begin
      Plugin.Data.Register (Name, Provider);
   end Register;

   procedure Get_Sitemap (Plugin : in out SEO_Module;
                          Name   : in String;
                          List   : in out Sitemap_Entry_Vectors.Vector;
                          Found  : out Boolean) is
   begin
      Plugin.Data.Get_Sitemap (Name, List, Found);
   end Get_Sitemap;

   procedure Get_Sitemap_Index (Plugin : in out SEO_Module;
                                Index  : in out Sitemap_Index_Vector) is
   begin
      Plugin.Data.Get_Sitemap_Index (Index);
   end Get_Sitemap_Index;

   use type Ada.Strings.Unbounded.Unbounded_String;

   protected body Seo_Data is

      procedure Register (Name     : in String;
                          Provider : Sitemap_Provider_Access) is
      begin
         Sitemaps.Append ((To_UString (Name), Provider, others => <>));
      end Register;

      --  Get the list of sitemap entries for the given sitemap name.
      procedure Get_Sitemap (Name  : in String;
                             List  : in out Sitemap_Entry_Vectors.Vector;
                             Found : out Boolean) is
      begin
         --  For now, we don't expect a lot of sitemap providers.
         for Iter in Sitemaps.Iterate loop
            declare
               Sitemap : constant Sitemap_Record_Vectors.Reference_Type
                 := Sitemaps.Reference (Iter);
            begin
               if Sitemap.Name = Name then
                  declare
                     use type Ada.Calendar.Time;
                     Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
                  begin
                     if not Sitemap.Has_Data or else Sitemap.Date + Refresh_Delay < Now then
                        Sitemap.Provider.Create_Sitemap (Sitemap.Sitemap);
                        Sitemap.Has_Data := True;
                        Sitemap.Date := Now;
                     end if;
                  end;
                  List := Sitemap.Sitemap.Entries;
                  Found := True;
                  return;
               end if;
            end;
         end loop;
         Found := False;
      end Get_Sitemap;

      procedure Get_Sitemap_Index (Index : in out Sitemap_Index_Vector) is
      begin
         for Iter in Sitemaps.Iterate loop
            declare
               use type Ada.Calendar.Time;
               Sitemap : constant Sitemap_Record_Vectors.Reference_Type
                 := Sitemaps.Reference (Iter);
               Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            begin
               if not Sitemap.Has_Data or else Sitemap.Date + Refresh_Delay < Now then
                  Sitemap.Provider.Create_Sitemap (Sitemap.Sitemap);
                  Sitemap.Has_Data := True;
                  Sitemap.Date := Now;
               end if;
               Index.Append ((Sitemap.Name, Sitemap.Date));
            end;
         end loop;
      end Get_Sitemap_Index;

      procedure Set_Refresh_Delay (New_Delay : in Duration) is
      begin
         Refresh_Delay := New_Delay;
      end Set_Refresh_Delay;

   end Seo_Data;

end AWA.SEO.Modules;
