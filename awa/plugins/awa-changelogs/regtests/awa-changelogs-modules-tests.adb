-----------------------------------------------------------------------
--  awa-changelogs-tests -- Tests for changelogs
--  Copyright (C) 2014, 2015 Stephane Carrez
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

with Util.Test_Caller;

with Security.Contexts;
with AWA.Services.Contexts;
with AWA.Users.Models;
with AWA.Tests.Helpers.Users;
package body AWA.Changelogs.Modules.Tests is

   package Caller is new Util.Test_Caller (Test, "AWA.Changelogs");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Changelogs.Add_Log",
                       Test_Add_Log'Access);
   end Add_Tests;

   procedure Test_Add_Log (T : in out Test) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-changelog@test.com");
      declare
         Change_Manager : constant Changelog_Module_Access := Get_Changelog_Module;
         User           : constant AWA.Users.Models.User_Ref := Context.Get_User;
      begin
         T.Assert (Change_Manager /= null, "There is no changelog module");

         Change_Manager.Add_Log (User.Get_Id, "awa_user", "A first changelog for the user");
      end;
   end Test_Add_Log;

end AWA.Changelogs.Modules.Tests;
