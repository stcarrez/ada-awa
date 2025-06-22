-----------------------------------------------------------------------
--  awa-votes-modules-tests -- Unit tests for vote service
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Votes.Modules.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with record
      Manager : AWA.Votes.Modules.Vote_Module_Access;
   end record;

   --  Test vote.
   procedure Test_Vote_Up (T : in out Test);

   --  Test vote.
   procedure Test_Vote_Undo (T : in out Test);

end AWA.Votes.Modules.Tests;
