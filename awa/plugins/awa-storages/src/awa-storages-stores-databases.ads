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
with ADO.Sessions;

with AWA.Storages.Models;
package AWA.Storages.Stores.Databases is

   --  ------------------------------
   --  Storage Service
   --  ------------------------------
   type Database_Store is new AWA.Storages.Stores.Store with null record;

   procedure Save (Storage : in Database_Store;
                   Into    : in out AWA.Storages.Models.Storage_Ref'Class;
                   Path    : in String);

   procedure Load (Storage : in Database_Store;
                   From    : in AWA.Storages.Models.Storage_Ref'Class;
                   Into    : in String);

end AWA.Storages.Stores.Databases;
