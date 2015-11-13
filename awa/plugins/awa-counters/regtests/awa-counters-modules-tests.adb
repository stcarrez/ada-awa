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

with Util.Measures;
with Util.Test_Caller;
with AWA.Tests.Helpers;
with AWA.Tests.Helpers.Users;
with AWA.Services.Contexts;
with AWA.Counters.Definition;
with Security.Contexts;

package body AWA.Counters.Modules.Tests is

   package User_Counter is
      new AWA.Counters.Definition (AWA.Users.Models.USER_TABLE, "count");

   package Session_Counter is
      new AWA.Counters.Definition (AWA.Users.Models.SESSION_TABLE, "count");

   package Caller is new Util.Test_Caller (Test, "Counters.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Counters.Modules.Increment",
                       Test_Increment'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of a wiki space.
   --  ------------------------------
   procedure Test_Increment (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

      AWA.Counters.Increment (User_Counter.Index, Context.Get_User);
      T.Manager := AWA.Counters.Modules.Get_Counter_Module;
      T.Assert (T.Manager /= null, "There is no counter plugin");

      T.Manager.Flush;

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
         T.Manager.Flush;
         Util.Measures.Report (S, "AWA.Counters.Flush");
      end;
      --        T.Manager := AWA.Wikis.Modules.Get_Wiki_Module;
--        T.Assert (T.Manager /= null, "There is no wiki manager");
--
--        W.Set_Name ("Test wiki space");
--        T.Manager.Create_Wiki_Space (W);
--        T.Assert (W.Is_Inserted, "The new wiki space was not created");
--
--        W.Set_Name ("Test wiki space update");
--        W.Set_Is_Public (True);
--        T.Manager.Save_Wiki_Space (W);
--
--        T.Manager.Load_Wiki_Space (Wiki => W2,
--                                   Id   => W.Get_Id);
--        Util.Tests.Assert_Equals (T, "Test wiki space update", String '(W2.Get_Name),
--                                  "Invalid wiki space name");
   end Test_Increment;

   --  ------------------------------
   --  Test creation of a wiki page.
   --  ------------------------------
   procedure Test_Create_Wiki_Page (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

--        W.Set_Name ("Test wiki space");
--        T.Manager.Create_Wiki_Space (W);
--
--        P.Set_Name ("The page");
--        P.Set_Title ("The page title");
--        T.Manager.Create_Wiki_Page (W, P, C);
--        T.Assert (P.Is_Inserted, "The new wiki page was not created");

   end Test_Create_Wiki_Page;

   --  ------------------------------
   --  Test creation of a wiki page content.
   --  ------------------------------
   procedure Test_Create_Wiki_Content (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-wiki@test.com");

--        W.Set_Name ("Test wiki space");
--        T.Manager.Create_Wiki_Space (W);
--
--        P.Set_Name ("The page");
--        P.Set_Title ("The page title");
--        T.Manager.Create_Wiki_Page (W, P, C);
--
--        C.Set_Format (AWA.Wikis.Models.FORMAT_MARKDOWN);
--        C.Set_Content ("-- Title" & ASCII.LF & "A paragraph");
--        C.Set_Save_Comment ("A first version");
--        T.Manager.Create_Wiki_Content (P, C);
--        T.Assert (C.Is_Inserted, "The new wiki content was not created");
--        T.Assert (not C.Get_Author.Is_Null, "The wiki content has an author");
--        T.Assert (not C.Get_Page.Is_Null, "The wiki content is associated with the wiki page");

   end Test_Create_Wiki_Content;

end AWA.Counters.Modules.Tests;
