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
with AWA.Tests;

package AWA.Jobs.Services.Tests is

   procedure Work_1 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);
   procedure Work_2 (Job : in out AWA.Jobs.Services.Abstract_Job_Type'Class);

   package Work_1_Definition is new Work_Definition (Work_1'Access);
   package Work_2_Definition is new Work_Definition (Work_2'Access);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new AWA.Tests.Test with null record;

   --  Test the job factory.
   procedure Test_Register (T : in out Test);

   type Test_Job is new AWA.Jobs.Services.Job_Type with null record;

   --  Execute the job.  This operation must be implemented and should perform the work
   --  represented by the job.  It should use the <tt>Get_Parameter</tt> function to retrieve
   --  the job parameter and it can use the <tt>Set_Result</tt> operation to save the result.
   overriding
   procedure Execute (Job : in out Test_Job);

   package Test_Definition is new AWA.Jobs.Services.Definition (Test_Job);

end AWA.Jobs.Services.Tests;
