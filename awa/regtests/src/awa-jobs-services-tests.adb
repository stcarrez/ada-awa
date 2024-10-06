-----------------------------------------------------------------------
--  jobs-tests -- Unit tests for AWA jobs
--  Copyright (C) 2012, 2013, 2014 Stephane Carrez
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

with Security.Contexts;
with AWA.Tests.Helpers.Users;
with AWA.Jobs.Modules;
with AWA.Services.Contexts;

package body AWA.Jobs.Services.Tests is

   package ASC renames AWA.Services.Contexts;

   My_Exception : exception;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Jobs.Services.Tests");

   package Caller is new Util.Test_Caller (Test, "Jobs.Services");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test AWA.Jobs.Modules.Register",
                       Test_Job_Schedule'Access);
      Caller.Add_Test (Suite, "Test AWA.Jobs.Schedule",
                       Test_Job_Execute_Get_Status'Access);
      Caller.Add_Test (Suite, "Test AWA.Jobs.Get_Parameter",
                       Test_Job_Execute_Get_Parameter'Access);
      Caller.Add_Test (Suite, "Test AWA.Jobs.Schedule with job exception",
                       Test_Job_Execute_Exception'Access);
      Caller.Add_Test (Suite, "Test AWA.Jobs.Schedule with user context",
                       Test_Job_Execute_Get_User'Access);
   end Add_Tests;

   procedure Work_1 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      Msg : constant String := Job.Get_Parameter ("message");
      Cnt : constant Natural := Job.Get_Parameter ("count", 0);
   begin
      Log.Info ("Execute work_1 {0}, count {1}", Msg, Natural'Image (Cnt));
   end Work_1;

   procedure Work_2 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      pragma Unreferenced (Job);
   begin
      Log.Info ("Execute work_2");
   end Work_2;

   --  ------------------------------
   --  Check the job status while the job is running.
   --  ------------------------------
   procedure Work_Check_Status (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      use type AWA.Jobs.Models.Job_Status_Type;

      Status : constant AWA.Jobs.Models.Job_Status_Type := Job.Get_Status;
   begin
      Log.Info ("Execute work_check_status");

      if Status /= AWA.Jobs.Models.RUNNING then
         Job.Set_Status (AWA.Jobs.Models.FAILED);
         Job.Set_Result ("result", "fail");
      else
         Job.Set_Result ("result", "ok");
      end if;
   end Work_Check_Status;

   --  ------------------------------
   --  Check the job parameter while the job is running.
   --  ------------------------------
   procedure Work_Check_Parameter (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      Count  : constant Natural := Job.Get_Parameter ("count", 0);
   begin
      Log.Info ("Execute work_check_status");

      if Count /= 1 then
         Job.Set_Status (AWA.Jobs.Models.FAILED);
         Job.Set_Result ("result", "fail");
      else
         Job.Set_Result ("result", "ok");
      end if;
   end Work_Check_Parameter;

   --  ------------------------------
   --  Check the job raising an exception.
   --  ------------------------------
   procedure Work_Check_Exception (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
   begin
      raise My_Exception;
   end Work_Check_Exception;

   --  ------------------------------
   --  Check the job is executed under the context of the user.
   --  ------------------------------
   procedure Work_Check_User (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class) is
      use type ASC.Service_Context_Access;
      use type ADO.Identifier;

      Ctx  : constant ASC.Service_Context_Access := ASC.Current;
      User : constant ADO.Identifier := ADO.Identifier (Job.Get_Parameter ("user", 0));
   begin
      if Ctx = null then
         Job.Set_Status (AWA.Jobs.Models.FAILED);
         Job.Set_Result ("result", "fail-no-context");
      elsif Ctx.Get_User.Is_Null then
         Job.Set_Status (AWA.Jobs.Models.FAILED);
         Job.Set_Result ("result", "fail-no-user");
      elsif Ctx.Get_User.Get_Id /= User then
         Job.Set_Status (AWA.Jobs.Models.FAILED);
         Job.Set_Result ("result", "fail-bad-user");
      else
         Job.Set_Status (AWA.Jobs.Models.TERMINATED);
         Job.Set_Result ("result", "ok");
      end if;
   end Work_Check_User;

   --  ------------------------------
   --  Wait for the job to be executed and verify its stats.
   --  ------------------------------
   procedure Wait_Job (T       : in out Test;
                       Id      : in ADO.Identifier;
                       Expect  : in AWA.Jobs.Models.Job_Status_Type) is
      use type AWA.Jobs.Models.Job_Status_Type;

      Context : AWA.Services.Contexts.Service_Context;
      Status  : AWA.Jobs.Models.Job_Status_Type;
   begin
      Context.Set_Context (AWA.Tests.Get_Application, null);

      --  Wait for the job to be executed
      for I in 1 .. 20 loop
         delay 0.5;
         Status := Get_Job_Status (Id);
         if Status = AWA.Jobs.Models.TERMINATED then
            T.Assert (Status = Expect, "Job status is not terminated");
            return;
         end if;
         if Status = AWA.Jobs.Models.FAILED then
            T.Assert (Status = Expect, "Job status is not terminated");
            return;
         end if;
      end loop;
      T.Assert (Status = AWA.Jobs.Models.TERMINATED, "Job execution time out");
   end Wait_Job;

   --  ------------------------------
   --  Test the job factory.
   --  ------------------------------
   procedure Run_Job (T       : in out Test;
                      Factory : in Job_Factory_Access;
                      Id      : out ADO.Identifier) is
      use type AWA.Jobs.Models.Job_Status_Type;
      use type ADO.Identifier;

      J       : AWA.Jobs.Services.Job_Type;
      M       : constant AWA.Jobs.Modules.Job_Module_Access := AWA.Jobs.Modules.Get_Job_Module;
      Context : AWA.Services.Contexts.Service_Context;
   begin
      Context.Set_Context (AWA.Tests.Get_Application, null);
      M.Register (Definition => Factory);

      J.Set_Parameter ("count", 1);
      Util.Tests.Assert_Equals (T, 1, J.Get_Parameter ("count", 0), "Invalid count param");

      J.Set_Parameter ("message", "Hello");
      Util.Tests.Assert_Equals (T, "Hello", J.Get_Parameter ("message"),
                                "Invalid message param");

      J.Schedule (Factory.all);

      T.Assert (J.Get_Status = AWA.Jobs.Models.SCHEDULED, "Job is not scheduled");
      T.Assert (J.Get_Identifier /= ADO.NO_IDENTIFIER, "Job has an identified");
      Id := J.Get_Identifier;
   end Run_Job;

   --  ------------------------------
   --  Test the Get_Status operation within a job.
   --  ------------------------------
   procedure Test_Job_Execute_Get_Status (T : in out Test) is
      Id : ADO.Identifier;
   begin
      Run_Job (T, Work_Check_Status_Definition.Factory, Id);
      Wait_Job (T, Id, AWA.Jobs.Models.TERMINATED);
   end Test_Job_Execute_Get_Status;

   --  ------------------------------
   --  Test the Get_Parameter operation within a job.
   --  ------------------------------
   procedure Test_Job_Execute_Get_Parameter (T : in out Test) is
      Id : ADO.Identifier;
   begin
      Run_Job (T, Work_Check_Parameter_Definition.Factory, Id);
      Wait_Job (T, Id, AWA.Jobs.Models.TERMINATED);
   end Test_Job_Execute_Get_Parameter;

   --  ------------------------------
   --  Test the job that raise some exception.
   --  ------------------------------
   procedure Test_Job_Execute_Exception (T : in out Test) is
      Id : ADO.Identifier;
   begin
      Run_Job (T, Work_Check_Exception_Definition.Factory, Id);
      Wait_Job (T, Id, AWA.Jobs.Models.FAILED);
   end Test_Job_Execute_Exception;

   --  ------------------------------
   --  Test the job and verify that the job is executed under the user who created the job.
   --  ------------------------------
   procedure Test_Job_Execute_Get_User (T : in out Test) is
      Id : ADO.Identifier;
      M        : constant AWA.Jobs.Modules.Job_Module_Access := AWA.Jobs.Modules.Get_Job_Module;
   begin
      declare
         Sec_Ctx  : Security.Contexts.Security_Context;
         Context  : AWA.Services.Contexts.Service_Context;
         J        : AWA.Jobs.Services.Job_Type;
      begin
         AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-job@test.com");
         M.Register (Definition =>  Work_Check_User_Definition.Factory);

         J.Set_Parameter ("count", 1);
         Util.Tests.Assert_Equals (T, 1, J.Get_Parameter ("count", 0), "Invalid count param");

         J.Set_Parameter ("user", Natural (Context.Get_User.Get_Id));

         J.Schedule (Work_Check_User_Definition.Factory.all);
         Id := J.Get_Identifier;
      end;
      Wait_Job (T, Id, AWA.Jobs.Models.TERMINATED);
   end Test_Job_Execute_Get_User;

   --  ------------------------------
   --  Test the job factory.
   --  ------------------------------
   procedure Test_Job_Schedule (T : in out Test) is
      use type AWA.Jobs.Models.Job_Status_Type;

      J       : AWA.Jobs.Services.Job_Type;
      M       : constant AWA.Jobs.Modules.Job_Module_Access := AWA.Jobs.Modules.Get_Job_Module;
      Context : AWA.Services.Contexts.Service_Context;
   begin
      Context.Set_Context (AWA.Tests.Get_Application, null);
      M.Register (Definition => Services.Tests.Work_1_Definition.Factory);

      J.Set_Parameter ("count", 1);
      Util.Tests.Assert_Equals (T, 1, J.Get_Parameter ("count", 0), "Invalid count param");

      J.Set_Parameter ("message", "Hello");
      Util.Tests.Assert_Equals (T, "Hello", J.Get_Parameter ("message"), "Invalid message param");

      J.Schedule (Work_1_Definition.Factory.all);

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
