-----------------------------------------------------------------------
--  awa-wikis-modules-tests -- Unit tests for wikis service
--  Copyright (C) 2015, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Measures;
with Util.Test_Caller;
with AWA.Tests.Helpers;
with AWA.Tests.Helpers.Users;
with AWA.Services.Contexts;
with AWA.Counters.Definition;
with AWA.Users.Models;
with Security.Contexts;

package body AWA.Counters.Modules.Tests is

   package User_Counter is
      new AWA.Counters.Definition (AWA.Users.Models.USER_TABLE);

   package Session_Counter is
      new AWA.Counters.Definition (AWA.Users.Models.SESSION_TABLE);

   package Global_Counter is
     new AWA.Counters.Definition (null, "count");

   package Caller is new Util.Test_Caller (Test, "Counters.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Counters.Modules.Increment",
                       Test_Increment'Access);
      Caller.Add_Test (Suite, "Test AWA.Counters.Modules.Increment (global counter)",
                       Test_Global_Counter'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test incrementing counters and flushing.
   --  ------------------------------
   procedure Test_Increment (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      Before    : Integer;
      After     : Integer;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

      T.Manager := AWA.Counters.Modules.Get_Counter_Module;
      T.Assert (T.Manager /= null, "There is no counter plugin");

      T.Manager.Get_Counter (User_Counter.Index, Context.Get_User, Before);
      AWA.Counters.Increment (User_Counter.Index, Context.Get_User);
      T.Manager.Flush;

      T.Manager.Get_Counter (User_Counter.Index, Context.Get_User, After);
      Util.Tests.Assert_Equals (T, Before + 1, After, "The counter must have been incremented");

      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            AWA.Counters.Increment (User_Counter.Index, Context.Get_User);
         end loop;
         Util.Measures.Report (S, "AWA.Counters.Increment", 1000);
      end;
      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            AWA.Counters.Increment (Session_Counter.Index, Context.Get_User_Session);
         end loop;
         Util.Measures.Report (S, "AWA.Counters.Increment", 1000);
      end;
      AWA.Counters.Increment (User_Counter.Index, Context.Get_User);
      declare
         S : Util.Measures.Stamp;
      begin
         T.Manager.Flush;
         Util.Measures.Report (S, "AWA.Counters.Flush");
      end;
      T.Manager.Get_Counter (User_Counter.Index, Context.Get_User, After);
      Util.Tests.Assert_Equals (T, Before + 2 + 1_000, After,
                                "The counter must have been incremented");
   end Test_Increment;

   --  ------------------------------
   --  Test creation of a wiki page.
   --  ------------------------------
   --  Test incrementing a global counter.
   procedure Test_Global_Counter (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

      T.Manager := AWA.Counters.Modules.Get_Counter_Module;
      T.Assert (T.Manager /= null, "There is no counter plugin");

      --  T.Manager.Get_Counter (Global_Counter.Index, Before);
      AWA.Counters.Increment (Global_Counter.Index);
      T.Manager.Flush;

   end Test_Global_Counter;

end AWA.Counters.Modules.Tests;
