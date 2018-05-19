# Jobs Module
The `AWA.Jobs` plugin defines a batch job framework for modules to perform and execute
long running and deferred actions.  The `Jobs` plugin is intended to help web application
designers in implementing end to end asynchronous operation.  A client schedules a job
and does not block nor wait for the immediate completion.  Instead, the client asks
periodically or uses other mechanisms to check for the job completion.

## Writing a job
A new job type is created by implementing the `Execute` operation of the abstract
`Job_Type` tagged record.

```Ada
type Resize_Job is new AWA.Jobs.Job_Type with ...;
```

The `Execute` procedure must be implemented.  It should use the `Get_Parameter` functions
to retrieve the job parameters and perform the work.  While the job is being executed,
it can save result by using the `Set_Result` operations, save messages by using the
`Set_Message` operations and report the progress by using `Set_Progress`.
It may report the job status by using `Set_Status`.

```Ada
procedure Execute (Job : in out Resize_Job) is
begin
    Job.Set_Result ("done", "ok");
end Execute;
```

## Registering a job
The `AWA.Jobs` plugin must be able to create the job instance when it is going to
be executed.  For this, a registration package must be instantiated:

```Ada
package Resize_Def is new AWA.Jobs.Definition (Resize_Job);
```

and the job definition must be added:

```Ada
AWA.Jobs.Modules.Register (Resize_Def.Create'Access);
```

## Scheduling a job
To schedule a job, declare an instance of the job to execute and set the job specific
parameters.  The job parameters will be saved in the database.  As soon as parameters
are defined, call the `Schedule` procedure to schedule the job in the job queue and
obtain a job identifier.

```Ada
Resize : Resize_Job;
...
Resize.Set_Parameter ("file", "image.png");
Resize.Set_Parameter ("width", "32");
Resize.Set_Parameter ("height, "32");
Resize.Schedule;
```

## Checking for job completion


## Job Module
The <b>Jobs.Modules</b> is the entry point for the management of asynchronous jobs.
It maintains a list of job types that can be executed for the application and it
manages the job dispatchers.

## Job Service
The <b>AWA.Jobs.Services</b> package defines the type abstractions and the core operation
to define a job operation procedure, create and schedule a job and perform the job work
when it is scheduled.






| Name           | Description                                                               |
| Name           | Description                                                               |
|:---------------|:--------------------------------------------------------------------------|
|:---------------|:--------------------------------------------------------------------------|
|jobHandler|The jobHandler is the bean that is created to execute a job.|
|jobHandler|The jobHandler is the bean that is created to execute a job.|


### Configuration
| Name                      | Description                                                    |
|:--------------------------|:---------------------------------------------------------------|
|jobs_queue|The job queue that execute the jobs|
| |long-running|



## Data Model
![](images/awa_jobs_model.png)


