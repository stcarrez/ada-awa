-----------------------------------------------------------------------
--  awa-images-beans -- Image Ada Beans
--  Copyright (C) 2016 Stephane Carrez
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

with ADO;

with Util.Beans.Objects;
with Util.Beans.Basic;
with AWA.Storages.Beans;
with AWA.Images.Models;
with AWA.Images.Modules;
package AWA.Images.Beans is

   FOLDER_ID_PARAMETER : constant String := "folderId";

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
   procedure Load_Files (Storage : in Image_List_Bean);

   overriding
   function Get_Value (List : in Image_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Create the Image_List_Bean bean instance.
   function Create_Image_List_Bean (Module : in AWA.Images.Modules.Image_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Images.Beans;
