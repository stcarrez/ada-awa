-----------------------------------------------------------------------
--  awa-images-modules -- Image management module
--  Copyright (C) 2012, 2016, 2020 Stephane Carrez
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
with Ada.Strings.Unbounded;

with Util.Processes;
with Util.Beans.Objects;
with Util.Log.Loggers;
with Util.Streams.Pipes;
with Util.Streams.Texts;
with Util.Strings;

with ADO.Sessions;

with EL.Variables.Default;
with EL.Contexts.Default;

with AWA.Modules.Get;
with AWA.Applications;
with AWA.Storages.Modules;
with AWA.Services.Contexts;
with AWA.Modules.Beans;
with AWA.Images.Beans;
package body AWA.Images.Modules is

   package ASC renames AWA.Services.Contexts;

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
      Plugin.Job_Module := AWA.Jobs.Modules.Get_Job_Module;
      if Plugin.Job_Module = null then
         Log.Error ("Cannot find the AWA Job module for the image thumbnail generation");
      else
         Plugin.Job_Module.Register (Definition => Thumbnail_Job_Definition.Factory);
      end if;
      Plugin.Thumbnail_Command := Plugin.Get_Config (PARAM_THUMBNAIL_COMMAND);
   end Configure;

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
            Ctx : constant ASC.Service_Context_Access := AWA.Services.Contexts.Current;
            DB  : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
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
         Image_Module'Class (Instance).Create_Image (Item);
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
         Image_Module'Class (Instance).Create_Image (Item);
      else
         Image_Module'Class (Instance).Delete_Image (Item);
      end if;
   end On_Update;

   --  ------------------------------
   --  The `On_Delete` procedure is called by `Notify_Delete` to notify the deletion of the item.
   --  ------------------------------
   overriding
   procedure On_Delete (Instance : in Image_Module;
                        Item     : in AWA.Storages.Models.Storage_Ref'Class) is
   begin
      Image_Module'Class (Instance).Delete_Image (Item);
   end On_Delete;

   --  ------------------------------
   --  Thumbnail job to identify the image dimension and produce a thumbnail.
   --  ------------------------------
   procedure Do_Thumbnail_Job (Plugin : in Image_Module;
                               Job    : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      Image_Id : constant ADO.Identifier := Job.Get_Parameter ("image_id");
   begin
      Image_Module'Class (Plugin).Build_Thumbnail (Image_Id);
   end Do_Thumbnail_Job;

   --  ------------------------------
   --  Get the image module instance associated with the current application.
   --  ------------------------------
   function Get_Image_Module return Image_Module_Access is
      function Get is new AWA.Modules.Get (Image_Module, Image_Module_Access, NAME);
   begin
      return Get;
   end Get_Image_Module;

   procedure Create_Thumbnail (Service : in Image_Module;
                               Source  : in String;
                               Into    : in String;
                               Width   : in out Natural;
                               Height  : in out Natural) is
      Ctx       : EL.Contexts.Default.Default_Context;
      Variables : aliased EL.Variables.Default.Default_Variable_Mapper;

      Proc    : Util.Processes.Process;
      Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
   begin
      Variables.Bind ("src", Util.Beans.Objects.To_Object (Source));
      Variables.Bind ("dst", Util.Beans.Objects.To_Object (Into));
      Variables.Bind ("width", Util.Beans.Objects.To_Object (Width));
      Variables.Bind ("height", Util.Beans.Objects.To_Object (Height));
      Ctx.Set_Variable_Mapper (Variables'Unchecked_Access);
      declare
         Cmd     : constant Util.Beans.Objects.Object := Service.Thumbnail_Command.Get_Value (Ctx);
         Command : constant String := Util.Beans.Objects.To_String (Cmd);
         Input   : Util.Streams.Texts.Reader_Stream;
      begin
         Width  := 0;
         Height := 0;
         Pipe.Open (Command, Util.Processes.READ_ALL);
         Input.Initialize (Pipe'Unchecked_Access, 1024);
         while not Input.Is_Eof loop
            declare
               use Ada.Strings;

               Line : Ada.Strings.Unbounded.Unbounded_String;
               Pos  : Natural;
               Sep  : Natural;
               Last : Natural;
            begin
               Input.Read_Line (Into  => Line, Strip => False);
               exit when Ada.Strings.Unbounded.Length (Line) = 0;
               Log.Info ("Received: {0}", Line);

               --  The '-verbose' option of ImageMagick reports information about the original
               --  image.  Extract the picture width and height.
               --  image.png PNG 120x282 120x282+0+0 8-bit DirectClass 34.4KB 0.000u 0:00.018
               Pos := Ada.Strings.Unbounded.Index (Line, " ");
               if Pos > 0 and Width = 0 then
                  Pos := Ada.Strings.Unbounded.Index (Line, " ", Pos + 1);
                  if Pos > 0 then
                     Sep := Ada.Strings.Unbounded.Index (Line, "x", Pos + 1);
                     Last := Ada.Strings.Unbounded.Index (Line, "=", Pos + 1);
                     if Sep > 0 and Sep < Last then
                        Log.Info ("Dimension {0} - {1}..{2}",
                                  Ada.Strings.Unbounded.Slice (Line, Pos, Last),
                                  Natural'Image (Pos), Natural'Image (Last));
                        Width := Natural'Value (Unbounded.Slice (Line, Pos + 1, Sep - 1));
                        Height := Natural'Value (Unbounded.Slice (Line, Sep + 1, Last - 1));
                     end if;
                  end if;
               end if;
            end;
         end loop;
         Pipe.Close;

         Util.Processes.Wait (Proc);
         if Pipe.Get_Exit_Status /= 0 then
            Log.Error ("Command {0} exited with status {1}", Command,
                       Integer'Image (Pipe.Get_Exit_Status));
         end if;
      end;
   end Create_Thumbnail;

   --  Build a thumbnail for the image identified by the Id.
   procedure Build_Thumbnail (Service : in Image_Module;
                              Id      : in ADO.Identifier) is
      Storage_Service : constant AWA.Storages.Services.Storage_Service_Access
        := AWA.Storages.Modules.Get_Storage_Manager;
      Ctx         : constant ASC.Service_Context_Access := ASC.Current;
      DB          : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Img         : AWA.Images.Models.Image_Ref;
      Thumb       : AWA.Images.Models.Image_Ref;
      Target_File : AWA.Storages.Storage_File (AWA.Storages.TMP);
      Local_File  : AWA.Storages.Storage_File (AWA.Storages.CACHE);
      Thumbnail   : AWA.Storages.Models.Storage_Ref;
      Width       : Natural := 64;
      Height      : Natural := 64;
   begin
      Img.Load (DB, Id);
      declare
         Image_File : constant AWA.Storages.Models.Storage_Ref'Class := Img.Get_Storage;
      begin
         Storage_Service.Get_Local_File (From => Image_File.Get_Id, Into => Local_File);
         Storage_Service.Create_Local_File (Target_File);

         Service.Create_Thumbnail (AWA.Storages.Get_Path (Local_File),
                                   AWA.Storages.Get_Path (Target_File), Width, Height);
         Thumbnail.Set_Mime_Type ("image/jpeg");
         Thumbnail.Set_Original (Image_File);
         Thumbnail.Set_Workspace (Image_File.Get_Workspace);
         Thumbnail.Set_Folder (Image_File.Get_Folder);
         Thumbnail.Set_Owner (Image_File.Get_Owner);
         Thumbnail.Set_Name (String '(Image_File.Get_Name));
         Storage_Service.Save (Thumbnail, AWA.Storages.Get_Path (Target_File),
                               AWA.Storages.Models.DATABASE);
         Thumb.Set_Width (64);
         Thumb.Set_Height (64);
         Thumb.Set_Owner (Image_File.Get_Owner);
         Thumb.Set_Folder (Image_File.Get_Folder);
         Thumb.Set_Storage (Thumbnail);
         Img.Set_Width (Width);
         Img.Set_Height (Height);
         Img.Set_Thumb_Width (64);
         Img.Set_Thumb_Height (64);
         Img.Set_Thumbnail (Thumbnail);
         Ctx.Start;
         Img.Save (DB);
         Thumb.Save (DB);
         Ctx.Commit;
      end;
   end Build_Thumbnail;

   --  Deletes the storage instance.
   procedure Delete_Image (Service : in Image_Module;
                           File    : in AWA.Storages.Models.Storage_Ref'Class) is
   begin
      null;
   end Delete_Image;

   --  ------------------------------
   --  Scale the image dimension.
   --  ------------------------------
   procedure Scale (Width     : in Natural;
                    Height    : in Natural;
                    To_Width  : in out Natural;
                    To_Height : in out Natural) is
   begin
      if To_Width = Natural'Last or To_Height = Natural'Last
        or (To_Width = 0 and To_Height = 0)
      then
         To_Width  := Width;
         To_Height := Height;
      elsif To_Width = 0 then
         To_Width := (Width * To_Height) / Height;
      elsif To_Height = 0 then
         To_Height := (Height * To_Width) / Width;
      end if;
   end Scale;

   --  ------------------------------
   --  Get the dimension represented by the string.  The string has one of the following
   --  formats:
   --    original          -> Width, Height := Natural'Last
   --    default           -> Width, Height := 0
   --    <width>x          -> Width := <width>, Height := 0
   --    x<height>         -> Width := 0, Height := <height>
   --    <width>x<height>  -> Width := <width>, Height := <height>
   --  ------------------------------
   procedure Get_Sizes (Dimension : in String;
                        Width     : out Natural;
                        Height    : out Natural) is
      Pos : Natural;
   begin
      if Dimension = "original" then
         Width  := Natural'Last;
         Height := Natural'Last;
      elsif Dimension = "default" then
         Width  := 800;
         Height := 0;
      else
         Pos := Util.Strings.Index (Dimension, 'x');
         if Pos > Dimension'First then
            begin
               Width := Natural'Value (Dimension (Dimension'First .. Pos - 1));

            exception
               when Constraint_Error =>
                  Width := 0;
            end;
         else
            Width := 0;
         end if;
         if Pos < Dimension'Last then
            begin
               Height := Natural'Value (Dimension (Pos + 1 .. Dimension'Last));

            exception
               when Constraint_Error =>
                  Height := 0;
            end;
         else
            Height := 0;
         end if;
      end if;
   end Get_Sizes;

end AWA.Images.Modules;
