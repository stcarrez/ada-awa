-----------------------------------------------------------------------
--  awa-storages-services-tests -- Unit tests for storage service
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

with AWA.Storages.Models;
package AWA.Storages.Services.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Id      : ADO.Identifier;
      Kind    : AWA.Storages.Models.Storage_Type := AWA.Storages.Models.DATABASE;
      Manager : AWA.Storages.Services.Storage_Service_Access;
   end record;

   --  Test creation of a storage object
   procedure Test_Create_Storage (T : in out Test);
   procedure Test_File_Create_Storage (T : in out Test);

   --  Test deletion of a storage object
   procedure Test_Delete_Storage (T : in out Test);

   --  Test creation of a storage folder
   procedure Test_Create_Folder (T : in out Test);

   --  Test creation of a storage object and local file access after its creation.
   procedure Test_Get_Local_File (T : in out Test);

   --  Save something in a storage element and keep track of the store id in the test <b>Id</b>.
   procedure Save (T : in out Test);

   --  Load the storage element refered to by the test <b>Id</b>.
   procedure Load (T : in out Test);

end AWA.Storages.Services.Tests;
