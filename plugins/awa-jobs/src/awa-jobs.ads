-----------------------------------------------------------------------
--  awa-jobs -- AWA Jobs
--  Copyright (C) 2012, 2015, 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  = Jobs Module =
--  The `jobs` module defines a batch job framework for modules to perform
--  and execute long running and deferred actions.  The `jobs` module is
--  intended to help web application designers in implementing end to end
--  asynchronous operation.  A client schedules a job and does not block
--  nor wait for the immediate completion.  Instead, the client asks
--  periodically or uses other mechanisms to check for the job completion.
--
--  @include awa-jobs-modules.ads
--
--  == Writing a job ==
--  A new job type is created by implementing the `Execute` operation
--  of the abstract `Job_Type` tagged record.
--
--    type Resize_Job is new AWA.Jobs.Job_Type with ...;
--
--  The `Execute` procedure must be implemented.  It should use the
--  `Get_Parameter` functions to retrieve the job parameters and perform
--  the work.  While the job is being executed, it can save result by using
--  the `Set_Result` operations, save messages by using the `Set_Message`
--  operations and report the progress by using `Set_Progress`.
--  It may report the job status by using `Set_Status`.
--
--    procedure Execute (Job : in out Resize_Job) is
--    begin
--        Job.Set_Result ("done", "ok");
--    end Execute;
--
--  == Registering a job ==
--  The `jobs` module must be able to create the job instance when
--  it is going to be executed.  For this, a registration package must
--  be instantiated:
--
--    package Resize_Def is new AWA.Jobs.Definition (Resize_Job);
--
--  and the job definition must be added:
--
--    AWA.Jobs.Modules.Register (Resize_Def.Create'Access);
--
--  == Scheduling a job ==
--  To schedule a job, declare an instance of the job to execute and set
--  the job specific parameters.  The job parameters will be saved in the
--  database.  As soon as parameters are defined, call the `Schedule`
--  procedure to schedule the job in the job queue and obtain a job identifier.
--
--    Resize : Resize_Job;
--    ...
--    Resize.Set_Parameter ("file", "image.png");
--    Resize.Set_Parameter ("width", "32");
--    Resize.Set_Parameter ("height, "32");
--    Resize.Schedule;
--
--  == Checking for job completion ==
--  After a job is scheduled, a unique identifier is allocated that allows
--  to identify it.  It is possible to query the status of the job by using
--  the `Get_Job_Status` function:
--
--    Status : AWA.Jobs.Models.Job_Status_Type
--      := AWA.Jobs.Services.Get_Job_Status (Resize.Get_Identifier);
--
--  @include awa-jobs-services.ads
--
--  == Ada Beans ==
--
--  @include-bean jobs.xml
--
--  == Data Model ==
--  [images/awa_jobs_model.png]
--
package AWA.Jobs is

   pragma Pure;

end AWA.Jobs;
