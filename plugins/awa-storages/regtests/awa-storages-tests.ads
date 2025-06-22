-----------------------------------------------------------------------
--  awa-storages-tests -- Unit tests for storages module
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Storages.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Folder_Ident  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Get some access on the wiki page as anonymous users.
   procedure Verify_Anonymous (T     : in out Test;
                               Page  : in String;
                               Title : in String);

   --  Verify that the wiki lists contain the given page.
   procedure Verify_List_Contains (T    : in out Test;
                                   Page : in String);

   --  Test access to the wiki as anonymous user.
   procedure Test_Anonymous_Access (T : in out Test);

   --  Test creation of document by simulating web requests.
   procedure Test_Create_Document (T : in out Test);

   --  Test getting a document which does not exist.
   procedure Test_Missing_Document (T : in out Test);

end AWA.Storages.Tests;
