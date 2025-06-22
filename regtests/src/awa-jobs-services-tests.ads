-----------------------------------------------------------------------
--  jobs-tests -- Unit tests for AWA jobs
--  Copyright (C) 2012, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with AWA.Tests;

package AWA.Jobs.Services.Tests is

   procedure Work_1 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);
   procedure Work_2 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   --  Check the job status while the job is running.
   procedure Work_Check_Status (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   --  Check the job parameter while the job is running.
   procedure Work_Check_Parameter (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   --  Check the job raising an exception.
   procedure Work_Check_Exception (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   --  Check the job is executed under the context of the user.
   procedure Work_Check_User (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   package Work_1_Definition is new Work_Definition (Work_1'Access);
   package Work_2_Definition is new Work_Definition (Work_2'Access);
   package Work_Check_Status_Definition is new Work_Definition (Work_Check_Status'Access);
   package Work_Check_Parameter_Definition is new Work_Definition (Work_Check_Parameter'Access);
   package Work_Check_Exception_Definition is new Work_Definition (Work_Check_Exception'Access);
   package Work_Check_User_Definition is new Work_Definition (Work_Check_User'Access);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   --  Test the job schedule.
   procedure Test_Job_Schedule (T : in out Test);

   --  Test the Get_Status operation within a job.
   procedure Test_Job_Execute_Get_Status (T : in out Test);

   --  Test the Get_Parameter operation within a job.
   procedure Test_Job_Execute_Get_Parameter (T : in out Test);

   --  Test the job that raise some exception.
   procedure Test_Job_Execute_Exception (T : in out Test);

   --  Test the job and verify that the job is executed under the user who created the job.
   procedure Test_Job_Execute_Get_User (T : in out Test);

   --  Wait for the job to be executed and verify its stats.
   procedure Wait_Job (T       : in out Test;
                       Id      : in ADO.Identifier;
                       Expect  : in AWA.Jobs.Models.Job_Status_Type);

   procedure Run_Job (T       : in out Test;
                      Factory : in Job_Factory_Access;
                      Id      : out ADO.Identifier);

   type Test_Job is new AWA.Jobs.Services.Abstract_Job_Type with null record;

   --  Execute the job.  This operation must be implemented and should perform the work
   --  represented by the job.  It should use the <tt>Get_Parameter</tt> function to retrieve
   --  the job parameter and it can use the <tt>Set_Result</tt> operation to save the result.
   overriding
   procedure Execute (Job : in out Test_Job);

   package Test_Definition is new AWA.Jobs.Services.Definition (Test_Job);

end AWA.Jobs.Services.Tests;
