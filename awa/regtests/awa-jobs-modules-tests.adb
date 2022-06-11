-----------------------------------------------------------------------
--  jobs-tests -- Unit tests for AWA jobs
--  Copyright (C) 2012, 2022 Stephane Carrez
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

with AWA.Jobs.Services.Tests;

package body AWA.Jobs.Modules.Tests is

   package Caller is new Util.Test_Caller (Test, "Jobs.Modules");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Jobs.Modules.Register",
                       Test_Register'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the job factory.
   --  ------------------------------
   procedure Test_Register (T : in out Test) is
      M : AWA.Jobs.Modules.Job_Module;
   begin
      M.Register (Definition => Services.Tests.Test_Definition.Factory);
      Util.Tests.Assert_Equals (T, 1, Integer (M.Factory.Length), "Invalid factory length");

      M.Register (Definition => Services.Tests.Work_1_Definition.Factory);
      Util.Tests.Assert_Equals (T, 2, Integer (M.Factory.Length), "Invalid factory length");

      M.Register (Definition => Services.Tests.Work_2_Definition.Factory);
      Util.Tests.Assert_Equals (T, 3, Integer (M.Factory.Length), "Invalid factory length");
   end Test_Register;

end AWA.Jobs.Modules.Tests;
