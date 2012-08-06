-----------------------------------------------------------------------
--  awa-images-services -- Image service
--  Copyright (C) 2012 Stephane Carrez
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

with Security.Permissions;

with Util.Processes;
with Util.Beans.Objects;
with Util.Log.Loggers;
with Util.Streams.Pipes;
with Util.Streams.Texts;

with ADO;
with AWA.Modules;
with AWA.Images.Models;
with AWA.Storages.Models;
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

      declare
         Ctx     : EL.Contexts.Default.Default_Context;
         Command : constant String := Module.Get_Config (PARAM_THUMBNAIL_COMMAND);
      begin
         Service.Thumbnail_Command := EL.Expressions.Create_Expression (Command, Ctx);

      exception
         when E : others =>
            Log.Error ("Invalid thumbnail command: ", E, True);
      end;
   end Initialize;

   procedure Create_Thumbnail (Service : in Image_Service;
                               Source  : in String;
                               Into    : in String;
                               Width   : out Natural;
                               Height  : out Natural) is
      Ctx       : EL.Contexts.Default.Default_Context;
      Variables : aliased EL.Variables.Default.Default_Variable_Mapper;

      Proc    : Util.Processes.Process;
      Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
   begin
      Variables.Bind ("src", Util.Beans.Objects.To_Object (Source));
      Variables.Bind ("dst", Util.Beans.Objects.To_Object (Into));
      Ctx.Set_Variable_Mapper (Variables'Unchecked_Access);
      declare
         Cmd     : constant Util.Beans.Objects.Object := Service.Thumbnail_Command.Get_Value (Ctx);
         Command : constant String := Util.Beans.Objects.To_String (Cmd);
         Input   : Util.Streams.Texts.Reader_Stream;
      begin
         Width  := 0;
         Height := 0;
         Pipe.Open (Command, Util.Processes.READ);
         Input.Initialize (null, Pipe'Unchecked_Access, 1024);
         while not Input.Is_Eof loop
            declare
               Line     : Ada.Strings.Unbounded.Unbounded_String;
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
                     Last := Ada.Strings.Unbounded.Index (Line, " ", Pos + 1);
                     if Sep > 0 and Sep < Last then
                        Width := Natural'Value (Ada.Strings.Unbounded.Slice (Line, Pos + 1, Sep - 1));
                        Height := Natural'Value (Ada.Strings.Unbounded.Slice (Line, Sep + 1, Last - 1));
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

   --  Save the data object contained in the <b>Data</b> part element into the
   --  target storage represented by <b>Into</b>.
   procedure Create_Image (Service : in Image_Service;
                           File    : in AWA.Storages.Models.Storage_Ref'Class) is
      Img : AWA.Images.Models.Image_Ref;
   begin
      Img.Set_Width (0);
      Img.Set_Height (0);
      Img.Set_Thumb_Height (0);
      Img.Set_Thumb_Width (0);
      Img.Set_Storage (File);
   end Create_Image;

   --  Deletes the storage instance.
   procedure Delete_Image (Service : in Image_Service;
                           File    : in AWA.Storages.Models.Storage_Ref'Class) is
   begin
      null;
   end Delete_Image;

end AWA.Images.Services;
