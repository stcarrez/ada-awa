-----------------------------------------------------------------------
--  AWA testsuite - AWA Testsuite
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Util.Properties;

with AWA.Applications;

package AWA.Testsuite is

   function Suite return Util.Tests.Access_Test_Suite;

   procedure Initialize (Props : in Util.Properties.Manager);

   --  Initialize the AWA test framework mockup.
   procedure Initialize (App         : in AWA.Applications.Application_Access;
                         Props       : in Util.Properties.Manager;
                         Add_Modules : in Boolean);

end AWA.Testsuite;
