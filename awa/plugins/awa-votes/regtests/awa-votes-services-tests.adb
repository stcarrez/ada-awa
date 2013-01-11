-----------------------------------------------------------------------
--  awa-questions-services-tests -- Unit tests for storage service
--  Copyright (C) 2013 Stephane Carrez
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
with Util.Beans.Basic;
with Util.Beans.Objects;

with ADO;
with ADO.Objects;

with Security.Contexts;

with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Votes.Modules;
with AWA.Tests.Helpers.Users;
package body AWA.Votes.Services.Tests is

   use Util.Tests;
   use ADO;

   package Caller is new Util.Test_Caller (Test, "Votes.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Votes.Services.Vote_Up",
                       Test_Vote_Up'Access);
    end Add_Tests;

   --  ------------------------------
   --  Test creation of a question.
   --  ------------------------------
   procedure Test_Vote_Up (T : in out Test) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
      Vote_Manager : Vote_Service;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-storage@test.com");

      declare
         User : AWA.Users.Models.User_Ref := Context.Get_User;
      begin
         Vote_Manager.Vote_For (User, 1);
         Vote_Manager.Vote_For (User, 2);
      end;
  end Test_Vote_Up;

end AWA.Votes.Services.Tests;
