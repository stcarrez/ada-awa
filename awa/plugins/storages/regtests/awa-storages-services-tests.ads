-----------------------------------------------------------------------
--  awa-storages-services-tests -- Unit tests for storage service
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

with Util.Tests;
with AWA.Tests;

package AWA.Storages.Services.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Id      : ADO.Identifier;
      Manager : AWA.Storages.Services.Storage_Service_Access;
   end record;

   --  Test creation of a storage object
   procedure Test_Create_Storage (T : in out Test);

   --  Test deletion of a storage object
   procedure Test_Delete_Storage (T : in out Test);

   --  Save something in a storage element and keep track of the store id in the test <b>Id</b>.
   procedure Save (T : in out Test);

   --  Load the storage element refered to by the test <b>Id</b>.
   procedure Load (T : in out Test);

end AWA.Storages.Services.Tests;
