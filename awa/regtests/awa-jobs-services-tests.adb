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

with Util.Test_Caller;
with Util.Log.Loggers;

with AWA.Jobs.Modules;
with AWA.Services.Contexts;

package body AWA.Jobs.Services.Tests is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Jobs.Services.Tests");

   package Caller is new Util.Test_Caller (Test, "Jobs.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Jobs.Modules.Register",
                       Test_Job_Schedule'Access);
   end Add_Tests;

   procedure Work_1 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      Msg : constant String := Job.Get_Parameter ("message");
      Cnt : constant Natural := Job.Get_Parameter ("count", 0);
   begin
      Log.Info ("Execute work_1 {0}, count {1}", Msg, Natural'Image (Cnt));
   end Work_1;

   procedure Work_2 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
   begin
      null;
   end Work_2;

   --  ------------------------------
   --  Test the job factory.
   --  ------------------------------
   procedure Test_Job_Schedule (T : in out Test) is
      use type AWA.Jobs.Models.Job_Status_Type;

      J       : AWA.Jobs.Services.Job_Type;
      M       : AWA.Jobs.Modules.Job_Module_Access := AWA.Jobs.Modules.Get_Job_Module;
      Context : AWA.Services.Contexts.Service_Context;
   begin
      Context.Set_Context (AWA.Tests.Get_Application, null);
      M.Register (Definition => Services.Tests.Work_1_Definition.Factory);

      J.Set_Parameter ("count", 1);
      Util.Tests.Assert_Equals (T, 1, J.Get_Parameter ("count", 0), "Invalid count param");

      J.Set_Parameter ("message", "Hello");
      Util.Tests.Assert_Equals (T, "Hello", J.Get_Parameter ("message"), "Invalid message param");

      J.Schedule (Work_1_Definition.Factory);

      T.Assert (J.Get_Status = AWA.Jobs.Models.SCHEDULED, "Job is not scheduled");
      --
      for I in 1 .. 10 loop
         delay 0.1;
      end loop;
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
