-----------------------------------------------------------------------
--  awa-seo-modules -- Search Engine Optimization module
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Applications;
with Ada.Calendar;
with Ada.Containers.Vectors;

with AWA.Modules;
private with AWA.SEO.Servlets;

--  == Integration ==
--  To be able to use the `SEO` module, you will need to add the following line in your
--  GNAT project file:
--
--    with "awa_seo";
--
--  The `SEO_Module` type manages the registration of sitemap providers, the registration
--  of the sitmap servlet and the generation of sitemaps.  Sitemap entries are cached globally
--  by the module in a protected object.  An instance of the `SEO_Module` must be declared
--  and registered in the AWA application.  The module instance can be defined as follows:
--
--    with AWA.SEO.Modules;
--    ...
--    type Application is new AWA.Applications.Application with record
--       SEO_Module : aliased AWA.SEO.Modules.SEO_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.SEO.Modules.NAME,
--              URI    => "seo",
--              Module => App.SEO_Module'Access);
--
--  the `SEO_Module` should be registered before a module that provides a sitemap provider.
--
--  == Configuration ==
--  The `SEO` module defines the following configuration parameters:
--
--  @include-config seo.xml
package AWA.SEO.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "seo";

   --  The configuration parameter that defines the URL base prefix to be used
   --  for sitemap XML files.
   PARAM_SITEMAP_PREFIX : constant String := "sitemap_prefix";

   --  The configuration parameter that indicates the refresh delay for sitemap.
   PARAM_SITEMAP_REFRESH : constant String := "sitemap_refresh";

   --  ------------------------------
   --  Module seo
   --  ------------------------------
   type SEO_Module is new AWA.Modules.Module with private;
   type SEO_Module_Access is access all SEO_Module'Class;

   --  Initialize the SEO module.
   overriding
   procedure Initialize (Plugin : in out SEO_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Configures the module after its initialization and after having read its XML configuration.
   overriding
   procedure Configure (Plugin : in out SEO_Module;
                        Props  : in ASF.Applications.Config);

   --  Get the seo module.
   function Get_SEO_Module return SEO_Module_Access;

   --  Register the sitemap provider under the given name.
   --  The provider will be called for the generation of sitemap.
   procedure Register (Plugin   : in out SEO_Module;
                       Name     : in String;
                       Provider : in Sitemap_Provider_Access);

   --  Get the sitemap entries that corresponds to the given name.
   procedure Get_Sitemap (Plugin : in out SEO_Module;
                          Name   : in String;
                          List   : in out Sitemap_Entry_Vectors.Vector;
                          Found  : out Boolean);

   --  Get the list of sitemap entries.
   procedure Get_Sitemap_Index (Plugin : in out SEO_Module;
                                Index  : in out Sitemap_Index_Vector);

private

   type Sitemap_Record is record
      Name     : UString;
      Provider : Sitemap_Provider_Access;
      Sitemap  : Sitemap_Info;
      Date     : Ada.Calendar.Time;
      Has_Data : Boolean := False;
   end record;

   package Sitemap_Record_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Sitemap_Record);

   protected type Seo_Data is
      procedure Register (Name     : in String;
                          Provider : Sitemap_Provider_Access);

      --  Get the list of sitemap entries for the given sitemap name.
      procedure Get_Sitemap (Name  : in String;
                             List  : in out Sitemap_Entry_Vectors.Vector;
                             Found : out Boolean);

      procedure Get_Sitemap_Index (Index : in out Sitemap_Index_Vector);

      procedure Set_Refresh_Delay (New_Delay : in Duration);

   private
      Refresh_Delay : Duration := 3600.0;
      Sitemaps      : Sitemap_Record_Vectors.Vector;
   end Seo_Data;

   type SEO_Module is new AWA.Modules.Module with record
      SEO_Servlet : aliased AWA.SEO.Servlets.SEO_Servlet;
      Data        : Seo_Data;
   end record;

end AWA.SEO.Modules;
