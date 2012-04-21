-----------------------------------------------------------------------
--  awa-storages-services -- Storage service
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
with AWA.Modules;

with AWA.Storages.Models;
package AWA.Storages.Services is

   --  ------------------------------
   --  Storage Service
   --  ------------------------------
   --  The <b>Storage_Service</b> defines a set of operations to store and retrieve
   --  a data object from the persistent storage.  The data object is treated as a raw
   --  byte stream.  The persistent storage can be implemented by a database, a file
   --  system or a remote service such as Amazon AWS.
   type Storage_Service is new AWA.Modules.Module_Manager with private;

   --  Save the data object contained in the <b>Data</b> part element into the
   --  target storage represented by <b>Into</b>.
   procedure Save (Service : in Storage_Service;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Data    : in ASF.Parts.Part'Class);

   --  Save the file pointed to by the <b>Path</b> string in the storage
   --  object represented by <b>Into</b> and managed by the storage service.
   procedure Save (Service : in Storage_Service;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String);

   --  Load the storage content identified by <b>From</b> in a local file
   --  that will be identified by <b>Into</b>.
   procedure Load (Service : in Storage_Service;
                   Into    : in out AWA.Storages.Models.Store_Local_Ref'Class;
                   From    : in AWA.Storages.Models.Storage_Ref'Class);

private

   type Storage_Service is new AWA.Modules.Module_Manager with record
      A : Natural;
   end record;

end AWA.Storages.Services;
