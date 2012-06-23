-----------------------------------------------------------------------
--  jobs-tests -- Unit tests for AWA jobs
--  Copyright (C) 2012 Stephane Carrez
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
with AWA.Jobs.Modules;
with AWA.Services.Contexts;

package body AWA.Jobs.Services.Tests is

   package Caller is new Util.Test_Caller (Test, "Jobs.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Jobs.Modules.Register",
                       Test_Job_Schedule'Access);
   end Add_Tests;

   procedure Work_1 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
   begin
      null;
   end Work_1;

   procedure Work_2 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
   begin
      null;
   end Work_2;

   --  Test the job factory.
   procedure Test_Job_Schedule (T : in out Test) is
      Context : AWA.Services.Contexts.Service_Context;
      J       : AWA.Jobs.Services.Job_Type;
   begin
      Context.Set_Context (AWA.Tests.Get_Application, null);

      J.Set_Parameter ("count", "1");
      J.Set_Parameter ("message", "Hello");
      J.Schedule (Work_1_Definition.Factory);

   end Test_Job_Schedule;

   --  Execute the job.  This operation must be implemented and should perform the work
   --  represented by the job.  It should use the <tt>Get_Parameter</tt> function to retrieve
   --  the job parameter and it can use the <tt>Set_Result</tt> operation to save the result.
   overriding
   procedure Execute (Job : in out Test_Job) is
   begin
      null;
   end Execute;

end AWA.Jobs.Services.Tests;
