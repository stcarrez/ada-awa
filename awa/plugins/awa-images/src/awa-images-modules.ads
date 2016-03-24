-----------------------------------------------------------------------
--  awa-images-modules -- Image management module
--  Copyright (C) 2012, 2016 Stephane Carrez
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
with AWA.Images.Services;
with AWA.Images.Models;
with AWA.Jobs.Services;
with AWA.Jobs.Modules;

--  == Image Module ==
--  The <tt>Image_Module</tt> type represents the image module.  An instance of the image
--  module must be declared and registered when the application is created and initialized.
--  The image module is associated with the image service which provides and implements
--  the image management operations.
--
--  When the image module is initialized, it registers itself as a listener to the storage
--  module to be notified when a storage file is created, updated or removed.  When a file
--  is added, it looks at the file type and extracts the image information if the storage file
--  is an image.
package AWA.Images.Modules is

   NAME : constant String := "images";

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

   --  Get the image manager.
   function Get_Image_Manager (Plugin : in Image_Module)
                                 return Services.Image_Service_Access;

   --  Create an image manager.  This operation can be overridden to provide another
   --  image service implementation.
   function Create_Image_Manager (Plugin : in Image_Module)
                                    return Services.Image_Service_Access;

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

   --  Get the image manager instance associated with the current application.
   function Get_Image_Manager return Services.Image_Service_Access;

   --  Returns true if the storage file has an image mime type.
   function Is_Image (File : in AWA.Storages.Models.Storage_Ref'Class) return Boolean;

private

   type Image_Module is new AWA.Modules.Module and AWA.Storages.Services.Listener with record
      Manager    : Services.Image_Service_Access := null;
      Job_Module : AWA.Jobs.Modules.Job_Module_Access;
   end record;

   --  Create an image instance.
   procedure Create_Image (Plugin  : in Image_Module;
                           File    : in AWA.Storages.Models.Storage_Ref'Class);

end AWA.Images.Modules;
