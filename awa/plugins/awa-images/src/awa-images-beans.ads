-----------------------------------------------------------------------
--  awa-images-beans -- Image Ada Beans
--  Copyright (C) 2016, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

with Util.Beans.Objects;
with Util.Beans.Basic;
with AWA.Storages.Beans;
with AWA.Images.Models;
with AWA.Images.Modules;

--  == Ada Beans ==
--  The `Image_List_Bean` type is used to represent a list of image stored in
--  a folder.
--
--  The `Image_Bean` type holds all the data to give information about an image.
--
--  @include-bean images.xml
--  @include-bean image-info.xml
--  @include-bean image-list.xml
package AWA.Images.Beans is

   --  ------------------------------
   --  Image List Bean
   --  ------------------------------
   --  This bean represents a list of images for a given folder.
   type Image_List_Bean is new AWA.Storages.Beans.Storage_List_Bean with record

      --  List of images.
      Image_List       : aliased AWA.Images.Models.Image_Info_List_Bean;
      Image_List_Bean  : AWA.Images.Models.Image_Info_List_Bean_Access;
   end record;
   type Image_List_Bean_Access is access all Image_List_Bean'Class;

   --  Load the list of images associated with the current folder.
   overriding
   procedure Load_Files (Storage : in out Image_List_Bean);

   overriding
   function Get_Value (List : in Image_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Create the Image_List_Bean bean instance.
   function Create_Image_List_Bean (Module : in AWA.Images.Modules.Image_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Image Bean
   --  ------------------------------
   --  Information about an image (excluding the image data itself).
   type Image_Bean is new AWA.Images.Models.Image_Bean with record
      Module : AWA.Images.Modules.Image_Module_Access;
   end record;
   type Image_Bean_Access is access all Image_Bean'Class;

   overriding
   procedure Load (Into    : in out Image_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Image_Bean bean instance.
   function Create_Image_Bean (Module : in AWA.Images.Modules.Image_Module_Access)
                               return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Images.Beans;
