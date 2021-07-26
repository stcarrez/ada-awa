# Jobs Module
The `jobs` module defines a batch job framework for modules to perform
and execute long running and deferred actions.  The `jobs` module is
intended to help web application designers in implementing end to end
asynchronous operation.  A client schedules a job and does not block
nor wait for the immediate completion.  Instead, the client asks
periodically or uses other mechanisms to check for the job completion.

## Integration
To be able to use the `jobs` module, you will need to add the
following line in your GNAT project file:

```Ada
with "awa_jobs";
```

An instance of the `Job_Module` must be declared and registered in the
AWA application.  The module instance can be defined as follows:

```Ada
with AWA.Jobs.Modules;
...
type Application is new AWA.Applications.Application with record
   Job_Module : aliased AWA.Jobs.Modules.Job_Module;
end record;
```

And registered in the `Initialize_Modules` procedure by using:

```Ada
Register (App    => App.Self.all'Access,
          Name   => AWA.Jobs.Modules.NAME,
          Module => App.Job_Module'Access);
```

## Writing a job
A new job type is created by implementing the `Execute` operation
of the abstract `Job_Type` tagged record.

```Ada
type Resize_Job is new AWA.Jobs.Job_Type with ...;
```

The `Execute` procedure must be implemented.  It should use the
`Get_Parameter` functions to retrieve the job parameters and perform
the work.  While the job is being executed, it can save result by using
the `Set_Result` operations, save messages by using the `Set_Message`
operations and report the progress by using `Set_Progress`.
It may report the job status by using `Set_Status`.

```Ada
procedure Execute (Job : in out Resize_Job) is
begin
    Job.Set_Result ("done", "ok");
end Execute;
```

## Registering a job
The `jobs` module must be able to create the job instance when
it is going to be executed.  For this, a registration package must
be instantiated:

```Ada
package Resize_Def is new AWA.Jobs.Definition (Resize_Job);
```

and the job definition must be added:

```Ada
AWA.Jobs.Modules.Register (Resize_Def.Create'Access);
```

## Scheduling a job
To schedule a job, declare an instance of the job to execute and set
the job specific parameters.  The job parameters will be saved in the
database.  As soon as parameters are defined, call the `Schedule`
procedure to schedule the job in the job queue and obtain a job identifier.

```Ada
Resize : Resize_Job;
...
Resize.Set_Parameter ("file", "image.png");
Resize.Set_Parameter ("width", "32");
Resize.Set_Parameter ("height, "32");
Resize.Schedule;
```

## Checking for job completion
After a job is scheduled, a unique identifier is allocated that allows
to identify it.  It is possible to query the status of the job by using
the `Get_Job_Status` function:

```Ada
Status : AWA.Jobs.Models.Job_Status_Type
  := AWA.Jobs.Services.Get_Job_Status (Resize.Get_Identifier);
```

## Job Service
The `AWA.Jobs.Services` package defines the type abstractions and the
core operation to define a job operation procedure, create and schedule
a job and perform the job work when it is scheduled.



## Ada Beans


| Name           | Description                                                               |
|:---------------|:--------------------------------------------------------------------------|
|jobHandler|The jobHandler is the bean that is created to execute a job.|


## Data Model
![](images/awa_jobs_model.png)


