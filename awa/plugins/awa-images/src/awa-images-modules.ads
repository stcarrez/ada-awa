-----------------------------------------------------------------------
--  awa-images-modules -- Image management module
--  Copyright (C) 2012, 2016, 2018, 2020 Stephane Carrez
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

with ASF.Applications;

with AWA.Modules;
with AWA.Storages.Models;
with AWA.Storages.Services;
with AWA.Images.Models;
with AWA.Jobs.Services;
with AWA.Jobs.Modules;
with AWA.Images.Servlets;
with ADO;
private with EL.Expressions;

--  == Integration ==
--  To be able to use the `Images` module, you will need to add the
--  following line in your GNAT project file:
--
--    with "awa_images";
--  
--  The `Image_Module` type represents the image module.  An instance
--  of the image module must be declared and registered when the application
--  is created and initialized.  The image module is associated with the image
--  service which provides and implements the image management operations.
--
--    with AWA.Images.Modules;
--    ...
--    type Application is new AWA.Applications.Application with record
--       Image_Module : aliased AWA.Images.Modules.Image_Module;
--    end record;
--
--  And it is registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Images.Modules.NAME,
--              Module => App.Image_Module'Access);
--
--  When the image module is initialized, it registers itself as a listener
--  to the storage module to be notified when a storage file is created,
--  updated or removed.  When a file is added, it looks at the file type
--  and extracts the image information if the storage file is an image.
--
--  == Configuration ==
--  The `Images` module defines the following configuration parameters:
--
--  @include-config images.xml
package AWA.Images.Modules is

   NAME : constant String := "images";

   PARAM_THUMBNAIL_COMMAND : constant String := "thumbnail_command";

   --  Job worker procedure to identify an image and generate its thumnbnail.
   procedure Thumbnail_Worker (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   package Thumbnail_Job_Definition is
     new AWA.Jobs.Services.Work_Definition (Thumbnail_Worker'Access);

   type Image_Module is new AWA.Modules.Module and AWA.Storages.Services.Listener with private;
   type Image_Module_Access is access all Image_Module'Class;

   --  Initialize the image module.
   overriding
   procedure Initialize (Plugin : in out Image_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Configures the module after its initialization and after having read its XML configuration.
   overriding
   procedure Configure (Plugin : in out Image_Module;
                        Props  : in ASF.Applications.Config);

   --  The `On_Create` procedure is called by `Notify_Create` to notify the creation of the item.
   overriding
   procedure On_Create (Instance : in Image_Module;
                        Item     : in AWA.Storages.Models.Storage_Ref'Class);

   --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the item.
   overriding
   procedure On_Update (Instance : in Image_Module;
                        Item     : in AWA.Storages.Models.Storage_Ref'Class);

   --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion of the item.
   overriding
   procedure On_Delete (Instance : in Image_Module;
                        Item     : in AWA.Storages.Models.Storage_Ref'Class);

   --  Create a thumbnail job for the image.
   procedure Make_Thumbnail_Job (Plugin : in Image_Module;
                                 Image  : in AWA.Images.Models.Image_Ref'Class);

   --  Thumbnail job to identify the image dimension and produce a thumbnail.
   procedure Do_Thumbnail_Job (Plugin : in Image_Module;
                               Job    : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   --  Get the image module instance associated with the current application.
   function Get_Image_Module return Image_Module_Access;

   --  Returns true if the storage file has an image mime type.
   function Is_Image (File : in AWA.Storages.Models.Storage_Ref'Class) return Boolean;

   procedure Create_Thumbnail (Service : in Image_Module;
                               Source  : in String;
                               Into    : in String;
                               Width   : in out Natural;
                               Height  : in out Natural);

   --  Build a thumbnail for the image identified by the Id.
   procedure Build_Thumbnail (Service : in Image_Module;
                              Id      : in ADO.Identifier);

   --  Save the data object contained in the <b>Data</b> part element into the
   --  target storage represented by <b>Into</b>.
   procedure Create_Image (Plugin : in Image_Module;
                           File    : in AWA.Storages.Models.Storage_Ref'Class);

   --  Deletes the storage instance.
   procedure Delete_Image (Service : in Image_Module;
                           File    : in AWA.Storages.Models.Storage_Ref'Class);

   --  Scale the image dimension.
   procedure Scale (Width     : in Natural;
                    Height    : in Natural;
                    To_Width  : in out Natural;
                    To_Height : in out Natural);

   --  Get the dimension represented by the string.  The string has one of the following
   --  formats:
   --    original          -> Width, Height := Natural'Last
   --    default           -> Width, Height := 0
   --    <width>x          -> Width := <width>, Height := 0
   --    x<height>         -> Width := 0, Height := <height>
   --    <width>x<height>  -> Width := <width>, Height := <height>
   procedure Get_Sizes (Dimension : in String;
                        Width     : out Natural;
                        Height    : out Natural);

private

   type Image_Module is new AWA.Modules.Module and AWA.Storages.Services.Listener with record
      Thumbnail_Command : EL.Expressions.Expression;
      Job_Module        : AWA.Jobs.Modules.Job_Module_Access;
      Image_Servlet     : aliased AWA.Images.Servlets.Image_Servlet;
   end record;

end AWA.Images.Modules;
