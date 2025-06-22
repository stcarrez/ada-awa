-----------------------------------------------------------------------
--  awa-jobs-services -- Job services
--  Copyright (C) 2012, 2014, 2015, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Objects.Maps;
with Util.Refs;

with ADO.Sessions;
with ADO.Objects;

with AWA.Events;
with AWA.Jobs.Models;

--  == Job Service ==
--  The `AWA.Jobs.Services` package defines the type abstractions and the
--  core operation to define a job operation procedure, create and schedule
--  a job and perform the job work when it is scheduled.
--
--  @type Abstract_Job_Type
--
--  @type
package AWA.Jobs.Services is

   --  The job is closed.  The status cannot be modified.
   Closed_Error   : exception;

   --  The job is already scheduled.
   Schedule_Error : exception;

   --  The job had an execution error.
   Execute_Error  : exception;

   --  The parameter value is invalid and cannot be set on the job instance.
   Invalid_Value  : exception;

   --  Event posted when a job is created.
   package Job_Create_Event is new AWA.Events.Definition (Name => "job-create");

   --  Get the job status.
   function Get_Job_Status (Id : in ADO.Identifier) return Models.Job_Status_Type;

   --  ------------------------------
   --  Abstract_Job Type
   --  ------------------------------
   --  The `Abstract_Job_Type` is an abstract tagged record which defines
   --  a job that can be scheduled and executed.  This is the base type of
   --  any job implementation.  It defines the `Execute` abstract procedure
   --  that must be implemented in concrete job types.
   --  It provides operation to setup and retrieve the job parameter.
   --  When the job `Execute` procedure is called, it allows to set the
   --  job execution status and result.
   type Abstract_Job_Type is abstract new Util.Refs.Ref_Entity
     and Util.Beans.Basic.Readonly_Bean with private;
   type Abstract_Job_Type_Access is access all Abstract_Job_Type'Class;

   type Work_Access is access procedure (Job : in out Abstract_Job_Type'Class);

   --  Execute the job.  This operation must be implemented and should
   --  perform the work represented by the job.  It should use the
   --  `Get_Parameter` function to retrieve the job parameter and it can
   --  use the `Set_Result` operation to save the result.
   procedure Execute (Job : in out Abstract_Job_Type) is abstract;

   --  Set the job parameter identified by the `Name` to the value
   --  given in `Value`.
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in String);

   --  Set the job parameter identified by the `Name` to the value
   --  given in `Value`.
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in Integer);

   --  Set the job parameter identified by the `Name` to the value
   --  given in `Value`.
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in ADO.Objects.Object_Ref'Class);

   --  Set the job parameter identified by the `Name` to the value
   --  given in `Value`.
   --  The value object can hold any kind of basic value type
   --  (integer, enum, date, strings).  If the value represents
   --  a bean, the `Invalid_Value` exception is raised.
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in Util.Beans.Objects.Object);

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (Job  : in Abstract_Job_Type;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Get the job parameter identified by the `Name` and convert
   --  the value into a string.
   function Get_Parameter (Job  : in Abstract_Job_Type;
                           Name : in String) return String;

   --  Get the job parameter identified by the `Name` and convert
   --  the value as an integer.  If the parameter is not defined,
   --  return the default value passed in `Default`.
   function Get_Parameter (Job     : in Abstract_Job_Type;
                           Name    : in String;
                           Default : in Integer) return Integer;

   --  Get the job parameter identified by the `Name` and convert
   --  the value as a database identifier.  If the parameter is not defined,
   --  return the `ADO.NO_IDENTIFIER`.
   function Get_Parameter (Job     : in Abstract_Job_Type;
                           Name    : in String) return ADO.Identifier;

   --  Get the job parameter identified by the `Name` and return it as
   --  a typed object.
   function Get_Parameter (Job  : in Abstract_Job_Type;
                           Name : in String) return Util.Beans.Objects.Object;

   --  Get the job status.
   function Get_Status (Job : in Abstract_Job_Type) return Models.Job_Status_Type;

   --  Get the job identifier once the job was scheduled.
   --  The job identifier allows to retrieve the job and check its
   --  execution and completion status later on.
   function Get_Identifier (Job : in Abstract_Job_Type) return ADO.Identifier;

   --  Set the job status.  When the job is terminated, it is closed
   --  and the job parameters or results cannot be changed.
   procedure Set_Status (Job    : in out Abstract_Job_Type;
                         Status : in AWA.Jobs.Models.Job_Status_Type);

   --  Set the job result identified by the `Name` to the value given
   --  in `Value`.  The value object can hold any kind of basic value
   --  type (integer, enum, date, strings).  If the value represents a bean,
   --  the `Invalid_Value` exception is raised.
   procedure Set_Result (Job   : in out Abstract_Job_Type;
                         Name  : in String;
                         Value : in Util.Beans.Objects.Object);

   --  Set the job result identified by the `Name` to the value given in `Value`.
   procedure Set_Result (Job   : in out Abstract_Job_Type;
                         Name  : in String;
                         Value : in String);

   --  Save the job information in the database.  Use the database session
   --  defined by `DB` to save the job.
   procedure Save (Job : in out Abstract_Job_Type;
                   DB  : in out ADO.Sessions.Master_Session'Class);

   --  ------------------------------
   --  Job Factory
   --  ------------------------------
   --  The `Job_Factory` is the interface that allows to create a job
   --  instance in order to execute a scheduled job.  The `Create` function
   --  is called to create a new job instance when the job is scheduled
   --  for execution.
   type Job_Factory is abstract tagged limited null record;
   type Job_Factory_Access is access all Job_Factory'Class;

   --  Create the job instance using the job factory.
   function Create (Factory : in Job_Factory) return Abstract_Job_Type_Access is abstract;

   --  Get the job factory name.
   function Get_Name (Factory : in Job_Factory'Class) return String;

   --  Schedule the job.
   procedure Schedule (Job        : in out Abstract_Job_Type;
                       Definition : in Job_Factory'Class);

   --  ------------------------------
   --  Job Type
   --  ------------------------------
   --  The `Job_Type` is a concrete job used by the `Work_Factory` to execute
   --  a simple `Work_Access` procedure.
   type Job_Type is new Abstract_Job_Type with private;

   overriding
   procedure Execute (Job : in out Job_Type);

   --  ------------------------------
   --  Work Factory
   --  ------------------------------
   --  The `Work_Factory` is a simplified `Job_Factory` that allows to register
   --  simple `Work_Access` procedures to execute the job.
   type Work_Factory (Work : Work_Access) is new Job_Factory with null record;

   --  Create the job instance to execute the associated `Work_Access` procedure.
   overriding
   function Create (Factory : in Work_Factory) return Abstract_Job_Type_Access;

   --  ------------------------------
   --  Job Declaration
   --  ------------------------------
   --  The `Definition` package must be instantiated with a given job type to
   --  register the new job definition.
   generic
      type T is new Abstract_Job_Type with private;
   package Definition is
      type Job_Type_Factory is new Job_Factory with null record;

      overriding
      function Create (Factory : in Job_Type_Factory) return Abstract_Job_Type_Access;

      --  The job factory.
      Factory : constant Job_Factory_Access;

   private
      Instance : aliased Job_Type_Factory;
      Factory  : constant Job_Factory_Access := Instance'Access;
   end Definition;

   generic
      Work : in Work_Access;
   package Work_Definition is
      type S_Factory is new Work_Factory with null record;

      --  The job factory.
      Factory : constant Job_Factory_Access;
   private
      Instance : aliased S_Factory := S_Factory '(Work => Work);
      Factory  : constant Job_Factory_Access := Instance'Access;
   end Work_Definition;

   type Job_Ref is private;

   --  Get the job parameter identified by the `Name` and return it as
   --  a typed object.  Return the `Null_Object` if the job is empty
   --  or there is no such parameter.
   function Get_Parameter (Job  : in Job_Ref;
                           Name : in String) return Util.Beans.Objects.Object;

   --  Execute the job associated with the given event.
   procedure Execute (Event  : in AWA.Events.Module_Event'Class;
                      Result : in out Job_Ref);

private

   --  Execute the job and save the job information in the database.
   procedure Execute (Job : in out Abstract_Job_Type'Class;
                      DB  : in out ADO.Sessions.Master_Session'Class);

   type Abstract_Job_Type is abstract new Util.Refs.Ref_Entity
     and Util.Beans.Basic.Readonly_Bean with record
      Job              : AWA.Jobs.Models.Job_Ref;
      Props            : Util.Beans.Objects.Maps.Map;
      Results          : Util.Beans.Objects.Maps.Map;
      Props_Modified   : Boolean := False;
      Results_Modified : Boolean := False;
   end record;

   --  ------------------------------
   --  Job Type
   --  ------------------------------
   --  The `Job_Type` is a concrete job used by the `Work_Factory` to execute
   --  a simple `Work_Access` procedure.
   type Job_Type is new Abstract_Job_Type with record
      Work : Work_Access;
   end record;

   package Job_Refs is
     new Util.Refs.Indefinite_References (Element_Type   => Abstract_Job_Type'Class,
                                          Element_Access => Abstract_Job_Type_Access);

   type Job_Ref is new Job_Refs.Ref with null record;

end AWA.Jobs.Services;
