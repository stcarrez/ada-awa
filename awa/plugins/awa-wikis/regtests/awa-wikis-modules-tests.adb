-----------------------------------------------------------------------
--  awa-wikis-modules-tests -- Unit tests for wikis service
--  Copyright (C) 2015 Stephane Carrez
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
with AWA.Tests;
with AWA.Tests.Helpers;
with AWA.Tests.Helpers.Users;
with AWA.Services.Contexts;
with Security.Contexts;

package body AWA.Wikis.Modules.Tests is

   package Caller is new Util.Test_Caller (Test, "Wikis.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Wikis.Modules.Create_Wiki_Space",
                       Test_Create_Wiki_Space'Access);
   end Add_Tests;

   --  Test creation of a wiki space.
   procedure Test_Create_Wiki_Space (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      W         : AWA.Wikis.Models.Wiki_Space_Ref;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

      T.Manager := AWA.Wikis.Modules.Get_Wiki_Module;
      T.Assert (T.Manager /= null, "There is no wiki manager");

      W.Set_Name ("Test wiki space");
      T.Manager.Create_Wiki_Space (W);
      T.Assert (W.Is_Inserted, "The new wiki space was not created");

   end Test_Create_Wiki_Space;

end AWA.Wikis.Modules.Tests;
