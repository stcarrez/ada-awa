-----------------------------------------------------------------------
--  awa-storages-beans-factories -- Factories for storage beans
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

with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;
package AWA.Storages.Beans.Factories is

   --  ------------------------------
   --  Upload Bean
   --  ------------------------------
   --  The <b>Upload_Bean</b> allows to upload a file in the storage space.
   type Upload_Bean is new AWA.Storages.Beans.Upload_Bean
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with null record;
   type Upload_Bean_Access is access all Upload_Bean'Class;

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Upload_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Create the Upload_Bean bean instance.
   function Create_Upload_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Folder Bean
   --  ------------------------------
   --  The <b>Folder_Bean</b> allows to create or update the folder name.
   type Folder_Bean is new AWA.Storages.Beans.Folder_Bean
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with null record;
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

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Folder_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Create the Folder_Bean bean instance.
   function Create_Folder_Bean (Module : in AWA.Storages.Modules.Storage_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Storages.Beans.Factories;
