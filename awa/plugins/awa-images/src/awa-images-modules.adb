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
with AWA.Modules.Get;
with AWA.Applications;
with AWA.Storages.Modules;
with AWA.Services.Contexts;
with AWA.Modules.Beans;
with AWA.Images.Beans;

with ADO.Sessions;

with Util.Strings;
with Util.Log.Loggers;
package body AWA.Images.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Images.Module");

   package Register is new AWA.Modules.Beans (Module        => Image_Module,
                                              Module_Access => Image_Module_Access);

   --  ------------------------------
   --  Job worker procedure to identify an image and generate its thumnbnail.
   --  ------------------------------
   procedure Thumbnail_Worker (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      Module : constant Image_Module_Access := Get_Image_Module;
   begin
      Module.Do_Thumbnail_Job (Job);
   end Thumbnail_Worker;

   --  ------------------------------
   --  Initialize the image module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Image_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the image module");

      --  Setup the resource bundles.
      App.Register ("imageMsg", "images");

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Images.Beans.Image_List_Bean",
                         Handler => AWA.Images.Beans.Create_Image_List_Bean'Access);

      Register.Register (Plugin  => Plugin,
                         Name    => "AWA.Images.Beans.Image_Bean",
                         Handler => AWA.Images.Beans.Create_Image_Bean'Access);

      App.Add_Servlet ("image", Plugin.Image_Servlet'Unchecked_Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      Plugin.Add_Listener (AWA.Storages.Modules.NAME, Plugin'Unchecked_Access);
   end Initialize;

   --  ------------------------------
   --  Configures the module after its initialization and after having read its XML configuration.
   --  ------------------------------
   overriding
   procedure Configure (Plugin : in out Image_Module;
                        Props  : in ASF.Applications.Config) is
      pragma Unreferenced (Props);

      use type AWA.Jobs.Modules.Job_Module_Access;
   begin
      --  Create the image manager when everything is initialized.
      Plugin.Manager := Plugin.Create_Image_Manager;
      Plugin.Job_Module := AWA.Jobs.Modules.Get_Job_Module;
      if Plugin.Job_Module = null then
         Log.Error ("Cannot find the AWA Job module for the image thumbnail generation");
      else
         Plugin.Job_Module.Register (Definition => Thumbnail_Job_Definition.Factory);
      end if;
   end Configure;

   --  ------------------------------
   --  Get the image manager.
   --  ------------------------------
   function Get_Image_Manager (Plugin : in Image_Module)
                                 return Services.Image_Service_Access is
   begin
      return Plugin.Manager;
   end Get_Image_Manager;

   --  ------------------------------
   --  Create an image manager.  This operation can be overridden to provide another
   --  image service implementation.
   --  ------------------------------
   function Create_Image_Manager (Plugin : in Image_Module)
                                    return Services.Image_Service_Access is
      Result : constant Services.Image_Service_Access := new Services.Image_Service;
   begin
      Result.Initialize (Plugin);
      return Result;
   end Create_Image_Manager;

   --  ------------------------------
   --  Create a thumbnail job for the image.
   --  ------------------------------
   procedure Make_Thumbnail_Job (Plugin : in Image_Module;
                                 Image  : in AWA.Images.Models.Image_Ref'Class) is
      pragma Unreferenced (Plugin);

      J : AWA.Jobs.Services.Job_Type;
   begin
      J.Set_Parameter ("image_id", Image);
      J.Schedule (Thumbnail_Job_Definition.Factory.all);
   end Make_Thumbnail_Job;

   --  ------------------------------
   --  Returns true if the storage file has an image mime type.
   --  ------------------------------
   function Is_Image (File : in AWA.Storages.Models.Storage_Ref'Class) return Boolean is
      Mime : constant String := File.Get_Mime_Type;
      Pos  : constant Natural := Util.Strings.Index (Mime, '/');
   begin
      if Pos = 0 then
         return False;
      else
         return Mime (Mime'First .. Pos - 1) = "image";
      end if;
   end Is_Image;

   --  ------------------------------
   --  Create an image instance.
   --  ------------------------------
   procedure Create_Image (Plugin  : in Image_Module;
                           File    : in AWA.Storages.Models.Storage_Ref'Class) is
   begin
      if File.Get_Original.Is_Null then
         declare
            Ctx : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
            DB  : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
            Img : AWA.Images.Models.Image_Ref;
         begin
            Img.Set_Width (0);
            Img.Set_Height (0);
            Img.Set_Thumb_Height (0);
            Img.Set_Thumb_Width (0);
            Img.Set_Storage (File);
            Img.Set_Folder (File.Get_Folder);
            Img.Set_Owner (File.Get_Owner);
            Img.Save (DB);
            Plugin.Make_Thumbnail_Job (Img);
         end;
      end if;
   end Create_Image;

   --  ------------------------------
   --  The `On_Create` procedure is called by `Notify_Create` to notify the creation of the item.
   --  ------------------------------
   overriding
   procedure On_Create (Instance : in Image_Module;
                        Item     : in AWA.Storages.Models.Storage_Ref'Class) is
   begin
      if Is_Image (Item) then
         Instance.Create_Image (Item);
      end if;
   end On_Create;

   --  ------------------------------
   --  The `On_Update` procedure is called by `Notify_Update` to notify the update of the item.
   --  ------------------------------
   overriding
   procedure On_Update (Instance : in Image_Module;
                        Item     : in AWA.Storages.Models.Storage_Ref'Class) is
   begin
      if Is_Image (Item) then
         Instance.Create_Image (Item);
      else
         Instance.Manager.Delete_Image (Item);
      end if;
   end On_Update;

   --  ------------------------------
   --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion of the item.
   --  ------------------------------
   overriding
   procedure On_Delete (Instance : in Image_Module;
                        Item     : in AWA.Storages.Models.Storage_Ref'Class) is
   begin
      Instance.Manager.Delete_Image (Item);
   end On_Delete;

   --  ------------------------------
   --  Thumbnail job to identify the image dimension and produce a thumbnail.
   --  ------------------------------
   procedure Do_Thumbnail_Job (Plugin : in Image_Module;
                               Job    : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      Image_Id : constant ADO.Identifier := Job.Get_Parameter ("image_id");
   begin
      Plugin.Manager.Build_Thumbnail (Image_Id);
   end Do_Thumbnail_Job;

   --  ------------------------------
   --  Get the image module instance associated with the current application.
   --  ------------------------------
   function Get_Image_Module return Image_Module_Access is
      function Get is new AWA.Modules.Get (Image_Module, Image_Module_Access, NAME);
   begin
      return Get;
   end Get_Image_Module;

   --  ------------------------------
   --  Get the image manager instance associated with the current application.
   --  ------------------------------
   function Get_Image_Manager return Services.Image_Service_Access is
      Module : constant Image_Module_Access := Get_Image_Module;
   begin
      if Module = null then
         Log.Error ("There is no active Storage_Module");
         return null;
      else
         return Module.Get_Image_Manager;
      end if;
   end Get_Image_Manager;

end AWA.Images.Modules;
