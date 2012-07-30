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

with ASF.Parts;
with ASF.Parts.Upload_Method;
with ASF.Events.Faces.Actions;

with AWA.Storages.Services;
package body AWA.Storages.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Upload_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return AWA.Storages.Models.Storage_Ref (From).Get_Value (Name);
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Upload_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      null;
   end Set_Value;

   --  ------------------------------
   --  Save the uploaded file in the storage service.
   --  ------------------------------
   procedure Save_Part (Bean : in out Upload_Bean;
                        Part : in ASF.Parts.Part'Class) is
      Manager : AWA.Storages.Services.Storage_Service_Access := Bean.Module.Get_Storage_Manager;
   begin
      Bean.Set_Name (Part.Get_Name);
      Bean.Set_Mime_Type (Part.Get_Content_Type);
      Bean.Set_File_Size (Part.Get_Size);
      Manager.Save (Bean, Part, AWA.Storages.Models.DATABASE);
   end Save_Part;

   --  Upload the file.
   procedure Upload (Bean    : in out Upload_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is

   begin
      null;
   end Upload;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Folder_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return AWA.Storages.Models.Storage_Folder_Ref (From).Get_Value (Name);
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Folder_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "name" then
         From.Set_Name (Name);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create or save the folder.
   --  ------------------------------
   procedure Save (Bean    : in out Folder_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Manager : AWA.Storages.Services.Storage_Service_Access := Bean.Module.Get_Storage_Manager;
   begin
      Manager.Save_Folder (Bean);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("success");
   end Save;

end AWA.Storages.Beans;
