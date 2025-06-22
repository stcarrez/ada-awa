-----------------------------------------------------------------------
--  awa-questions-services-tests -- Unit tests for storage service
--  Copyright (C) 2013, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;

with Security.Contexts;

with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Tests.Helpers.Users;
package body AWA.Votes.Modules.Tests is

   package Caller is new Util.Test_Caller (Test, "Votes.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Votes.Modules.Vote_For",
                       Test_Vote_Up'Access);
      Caller.Add_Test (Suite, "Test AWA.Votes.Modules.Vote_For (Undo)",
                       Test_Vote_Undo'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of a question.
   --  ------------------------------
   procedure Test_Vote_Up (T : in out Test) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-storage@test.com");

      declare
         Vote_Manager : constant Vote_Module_Access := Get_Vote_Module;
         User         : constant AWA.Users.Models.User_Ref := Context.Get_User;
         Total        : Integer;
      begin
         T.Assert (Vote_Manager /= null, "There is no vote module");

         Vote_Manager.Vote_For (User.Get_Id, "awa_user", "workspaces-create", 1, Total);
         T.Assert (Total > 0, "Invalid total");

         Vote_Manager.Vote_For (User.Get_Id, "awa_user", "workspaces-create", 2, Total);
         T.Assert (Total > 0, "Invalid total");
      end;
   end Test_Vote_Up;

   --  ------------------------------
   --  Test vote.
   --  ------------------------------
   procedure Test_Vote_Undo (T : in out Test) is
      Sec_Ctx      : Security.Contexts.Security_Context;
      Context      : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-vote@test.com");

      declare
         Vote_Manager : constant Vote_Module_Access := Get_Vote_Module;
         User         : constant AWA.Users.Models.User_Ref := Context.Get_User;
         Total        : Integer;
      begin
         T.Assert (Vote_Manager /= null, "There is no vote module");

         Vote_Manager.Vote_For (User.Get_Id, "awa_user", "workspaces-create", 1, Total);
         T.Assert (Total > 0, "Invalid total");

         Vote_Manager.Vote_For (User.Get_Id, "awa_user", "workspaces-create", 2, Total);
         T.Assert (Total > 0, "Invalid total");

         Vote_Manager.Vote_For (User.Get_Id, "awa_user", "workspaces-create", 0, Total);
         T.Assert (Total >= 0, "Invalid total");
      end;
   end Test_Vote_Undo;

end AWA.Votes.Modules.Tests;
