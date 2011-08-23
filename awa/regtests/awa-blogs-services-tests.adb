-----------------------------------------------------------------------
--  awa-blogs-tests -- Unit tests for blogs module
--  Copyright (C) 2011 Stephane Carrez
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
with Util.Test_Caller;
with Util.Measures;

with ADO;
with ADO.Sessions;
with ADO.SQL;
with ADO.Objects;
with Ada.Calendar;

with Security.Contexts;

with AWA.Services.Contexts;
with AWA.Blogs.Module;
with AWA.Users.Services.Tests.Helpers;
package body AWA.Blogs.Services.Tests is

   use Util.Tests;
   use ADO;
   use ADO.Objects;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Blogs.Services.Create_Blog",
                       Test_Create_Blog'Access);

   end Add_Tests;

   --  ------------------------------
   --  Test creation of a user
   --  ------------------------------
   procedure Test_Create_Blog (T : in out Test) is
      Manager   : AWA.Blogs.Services.Blog_Service_Access;
      Blog_Id   : ADO.Identifier;
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Users.Services.Tests.Helpers.Login (Context, Sec_Ctx, "test-blog@test.com");

      Manager := AWA.Blogs.Module.Get_Blog_Manager;
      Manager.Create_Blog (Workspace_Id => 0,
                           Title        => "My blog",
                           Result       => Blog_Id);
   end Test_Create_Blog;

end AWA.Blogs.Services.Tests;
