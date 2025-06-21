-----------------------------------------------------------------------
--  AWA - Unit tests
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Testsuite;

with Util.Tests;
with AWA.Tests;
procedure AWA_Harness is

   procedure Harness is new Util.Tests.Harness (AWA.Testsuite.Suite,
                                                AWA.Testsuite.Initialize,
                                                AWA.Tests.Finish);

begin
   Harness ("awa-tests.xml");
end AWA_Harness;
