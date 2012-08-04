-----------------------------------------------------------------------
--  awa-storages-beans -- Storage Ada Beans
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
with Ada.Strings.Unbounded;

with AWA.Storages.Models;
with AWA.Storages.Modules;

with ASF.Parts;

with Util.Beans.Objects;
with Util.Beans.Basic;
package AWA.Storages.Beans is

   FOLDER_ID_PARAMETER : constant String := "folderId";

   --  ------------------------------
   --  Upload Bean
   --  ------------------------------
   --  The <b>Upload_Bean</b> allows to upload a file in the storage space.
   type Upload_Bean is new AWA.Storages.Models.Storage_Ref
     and Util.Beans.Basic.Bean with record
      Module  : AWA.Storages.Modules.Storage_Module_Access := null;
   end record;
   type Upload_Bean_Access is access all Upload_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Upload_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Upload_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Save the uploaded file in the storage service.
   --  @method
   procedure Save_Part (Bean : in out Upload_Bean;
                        Part : in ASF.Parts.Part'Class);

   --  Upload the file.
   --  @method
   procedure Upload (Bean    : in out Upload_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Delete the file.
   --  @method
   procedure Delete (Bean    : in out Upload_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  ------------------------------
   --  Folder Bean
   --  ------------------------------
   --  The <b>Folder_Bean</b> allows to create or update the folder name.
   type Folder_Bean is new AWA.Storages.Models.Storage_Folder_Ref
     and Util.Beans.Basic.Bean with record
      Module  : AWA.Storages.Modules.Storage_Module_Access := null;
   end record;
   type Folder_Bean_Access is access all Folder_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Folder_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Folder_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create or save the folder.
   procedure Save (Bean    : in out Folder_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Folder_List_Bean bean instance.
   function Create_Folder_List_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access;

   --  Create the Storage_List_Bean bean instance.
   function Create_Storage_List_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                                      return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Storages.Beans;
