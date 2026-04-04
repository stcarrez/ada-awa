-----------------------------------------------------------------------
--  awa-seo -- Search Engine Optimization module
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Calendar;

--  = SEO Module =
--  The `AWA.SEO` module provides tools for search engine optimization when publishing
--  pages that must be discovered and scanned by search engines.  It provides `sitemap`
--  generation and allows other modules to announce what they publish for search engines
--  in the form of a sitemap entry.
--
--  A sitemap entry is described by the `Sitemap_Entry` type and it is composed of an
--  absolute URI location, a last modification date, a priority, an optional image URI
--  location and an optional image title.  A module that wants to provide a list of
--  sitemap entries must implement the `Sitemap_Provider` limited interface and implement
--  the `Create_Sitemap` procedure.  That procedure must populate a `Sitemap_Info` record
--  which contains the sitemap entries that must be exposed.  For example:
--
--    with AWA.SEO;
--    ...
--    procedure Create_Sitemap (Provider : in Sitemap_Provider;
--                              Sitemap  : in out Sitemap_Info) is
--       Item : Sitempap_Entry;
--    begin
--       ...
--       Sitemap.Entries.Append (Item);
--       ...
--    end Create_Sitemap;
--
--  The module that implements such sitemap provider must register itself to the `SEO` module
--  by using the `Register` procedure and giving the name of the sitemap file:
--
--    AWA.SEO.Register ("my-sitemap.xml", My_Provider'Access);
--
--  Such registration should be made in the `Configure` procedure of the module.
--
--  @include awa-seo-modules.ads
--  @include awa-seo-servlets.ads
package AWA.SEO is

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   function To_UString (Value : in String) return UString
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function To_String (Value : in UString) return String
     renames Ada.Strings.Unbounded.To_String;

   function Length (Value : in UString) return Natural
     renames Ada.Strings.Unbounded.Length;

   --  Describe an entry in the sitmap generation.
   type Sitemap_Entry is record
      Location    : UString;
      Date        : Ada.Calendar.Time;
      Priority    : Natural;
      Image       : UString;
      Image_Title : UString;
   end record;

   package Sitemap_Entry_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Sitemap_Entry);
   subtype Sitemap_Entry_Vector is Sitemap_Entry_Vectors.Vector;

   type Sitemap_Info is record
      Entries : Sitemap_Entry_Vectors.Vector;
   end record;

   type Sitemap_Index is record
      Location   : UString;
      Date       : Ada.Calendar.Time;
   end record;

   package Sitemap_Index_Vectors is
     new Ada.Containers.Vectors (Index_Type => Positive,
                                 Element_Type => Sitemap_Index);
   subtype Sitemap_Index_Vector is Sitemap_Index_Vectors.Vector;

   --  ------------------------------
   --  Sitmap provider interface responsible for producing the sitemap entries.
   --  ------------------------------
   type Sitemap_Provider is limited interface;
   type Sitemap_Provider_Access is access all Sitemap_Provider'Class;

   procedure Create_Sitemap (Provider : in Sitemap_Provider;
                             Sitemap  : in out Sitemap_Info) is abstract;

   --  Register the sitemap provider under the given name.
   procedure Register (Name     : in String;
                       Provider : in Sitemap_Provider_Access)
     with Pre => Name'Length > 0;

end AWA.SEO;
