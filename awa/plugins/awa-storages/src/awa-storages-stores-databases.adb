-----------------------------------------------------------------------
--  awa-storages-stores-databases -- Database store
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

package body AWA.Storages.Stores.Databases is

   --  ------------------------------
   --  Save the file represented by the `Path` variable into a store and associate that
   --  content with the storage reference represented by `Into`.
   --  ------------------------------
   procedure Save (Storage : in Database_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String) is
      pragma Unreferenced (Storage);

      Store : AWA.Storages.Models.Storage_Data_Ref;
      Blob  : constant ADO.Blob_Ref := ADO.Create_Blob (Path);
   begin
      Store.Set_Data (Blob);
      Store.Save (Session);
      Into.Set_Store_Data (Store);
   end Save;

   procedure Load (Storage : in Database_Store;
                   Session : in out ADO.Sessions.Master_Session;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in String) is
   begin
      null;
   end Load;

end AWA.Storages.Stores.Databases;
