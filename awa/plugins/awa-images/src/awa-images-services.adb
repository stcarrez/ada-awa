-----------------------------------------------------------------------
--  awa-images-services -- Image service
--  Copyright (C) 2012, 2013, 2015, 2016 Stephane Carrez
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

with Util.Processes;
with Util.Beans.Objects;
with Util.Log.Loggers;
with Util.Streams.Pipes;
with Util.Streams.Texts;
with Util.Strings;

with ADO.Sessions;

with AWA.Images.Models;
with AWA.Services.Contexts;
with AWA.Storages.Services;
with AWA.Storages.Modules;
with Ada.Strings.Unbounded;

with EL.Variables.Default;
with EL.Contexts.Default;

--  == Storage Service ==
--  The <tt>Storage_Service</tt> provides the operations to access and use the persisent storage.
--  It controls the permissions that grant access to the service for users.
--
--  Other modules can be notified of storage changes by registering a listener
--  on the storage module.
package body AWA.Images.Services is

   package ASC renames AWA.Services.Contexts;

   --  ------------------------------
   --  Image Service
   --  ------------------------------

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Images.Services");

   --  ------------------------------
   --  Initializes the storage service.
   --  ------------------------------
   overriding
   procedure Initialize (Service : in out Image_Service;
                         Module  : in AWA.Modules.Module'Class) is
   begin
      AWA.Modules.Module_Manager (Service).Initialize (Module);

      Service.Thumbnail_Command := Module.Get_Config (PARAM_THUMBNAIL_COMMAND);
   end Initialize;

   procedure Create_Thumbnail (Service : in Image_Service;
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
         Input.Initialize (null, Pipe'Unchecked_Access, 1024);
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
   procedure Build_Thumbnail (Service : in Image_Service;
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

   --  Save the data object contained in the <b>Data</b> part element into the
   --  target storage represented by <b>Into</b>.
   procedure Create_Image (Service : in Image_Service;
                           File    : in AWA.Storages.Models.Storage_Ref'Class) is
      pragma Unreferenced (Service);

      Ctx : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB  : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Img : AWA.Images.Models.Image_Ref;
   begin
      Ctx.Start;
      Img.Set_Width (0);
      Img.Set_Height (0);
      Img.Set_Thumb_Height (0);
      Img.Set_Thumb_Width (0);
      Img.Set_Storage (File);
      Img.Set_Folder (File.Get_Folder);
      Img.Set_Owner (File.Get_Owner);
      Img.Save (DB);
      Ctx.Commit;
   end Create_Image;

   --  Deletes the storage instance.
   procedure Delete_Image (Service : in Image_Service;
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
            Width := Natural'Value (Dimension (Dimension'First .. Pos - 1));
         else
            Width := 0;
         end if;
         if Pos < Dimension'Last then
            Height := Natural'Value (Dimension (Pos + 1 .. Dimension'Last));
         else
            Height := 0;
         end if;
      end if;
   end Get_Sizes;

end AWA.Images.Services;
