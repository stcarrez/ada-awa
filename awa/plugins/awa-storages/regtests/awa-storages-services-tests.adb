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
with Ada.Streams;
with Ada.Strings.Unbounded;

with Util.Test_Caller;

with ADO;
with ADO.Objects;

with Security.Contexts;

with AWA.Services.Contexts;
with AWA.Storages.Modules;
with AWA.Storages.Beans.Factories;
with AWA.Tests.Helpers.Users;
package body AWA.Storages.Services.Tests is

   use Util.Tests;
   use ADO;

   package Caller is new Util.Test_Caller (Test, "Storages.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Storages.Services.Save",
                       Test_Create_Storage'Access);
      Caller.Add_Test (Suite, "Test AWA.Storages.Services.Delete",
                       Test_Delete_Storage'Access);
      Caller.Add_Test (Suite, "Test AWA.Storages.Services.Save_Folder, Folder_Bean",
                       Test_Create_Folder'Access);

   end Add_Tests;

   --  ------------------------------
   --  Save something in a storage element and keep track of the store id in the test <b>Id</b>.
   --  ------------------------------
   procedure Save (T : in out Test) is
      Store     : AWA.Storages.Models.Storage_Ref;
   begin
      T.Manager := AWA.Storages.Modules.Get_Storage_Manager;
      T.Assert (T.Manager /= null, "Null storage manager");

      T.Manager.Save (Into    => Store,
                      Path    => "Makefile",
                      Storage => AWA.Storages.Models.DATABASE);
      T.Assert (not Store.Is_Null, "Storage object should not be null");
      T.Id := Store.Get_Id;
      T.Assert (T.Id > 0, "Invalid storage identifier");
   end Save;

   --  ------------------------------
   --  Load the storage element refered to by the test <b>Id</b>.
   --  ------------------------------
   procedure Load (T : in out Test) is
      use type Ada.Streams.Stream_Element_Offset;

      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Mime      : Ada.Strings.Unbounded.Unbounded_String;
      Date      : Ada.Calendar.Time;
      Data      : ADO.Blob_Ref;
   begin
      T.Manager := AWA.Storages.Modules.Get_Storage_Manager;
      T.Assert (T.Manager /= null, "Null storage manager");

      T.Manager.Load (From => T.Id, Name => Name, Mime => Mime, Date => Date, Into => Data);
      T.Assert (not Data.Is_Null, "Null blob returned by load");
      T.Assert (Data.Value.Len > 100, "Invalid length for the blob data");
   end Load;

   --  ------------------------------
   --  Test creation of a storage object
   --  ------------------------------
   procedure Test_Create_Storage (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-storage@test.com");

      T.Save;
      T.Load;
   end Test_Create_Storage;

   --  ------------------------------
   --  Test deletion of a storage object
   --  ------------------------------
   procedure Test_Delete_Storage (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Data      : ADO.Blob_Ref;
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Mime      : Ada.Strings.Unbounded.Unbounded_String;
      Date      : Ada.Calendar.Time;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-storage@test.com");

      T.Save;
      T.Manager.Delete (T.Id);
      begin
         T.Manager.Load (From => T.Id, Name => Name, Mime => Mime, Date => Date, Into => Data);
         T.Assert (False, "No exception raised");

      exception
         when ADO.Objects.NOT_FOUND =>
            null;
      end;
   end Test_Delete_Storage;

   --  ------------------------------
   --  Test creation of a storage folder
   --  ------------------------------
   procedure Test_Create_Folder (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Folder    : AWA.Storages.Beans.Factories.Folder_Bean;
      Outcome   : Ada.Strings.Unbounded.Unbounded_String;
      Upload    : AWA.Storages.Beans.Factories.Upload_Bean;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-storage@test.com");
      Folder.Module := AWA.Storages.Modules.Get_Storage_Module;
      Upload.Module := AWA.Storages.Modules.Get_Storage_Module;

      Folder.Set_Name ("Test folder name");
      Folder.Save (Outcome);
      Util.Tests.Assert_Equals (T, "success", Outcome, "Invalid outcome returned by Save action");

      Upload.Set_Value ("folderId", ADO.Objects.To_Object (Folder.Get_Key));
      Util.Tests.Assert_Equals (T, "Test folder name", String '(Upload.Get_Folder.Get_Name),
                                "Invalid folder name");
   end Test_Create_Folder;

end AWA.Storages.Services.Tests;
