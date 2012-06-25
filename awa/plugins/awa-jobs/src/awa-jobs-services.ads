-----------------------------------------------------------------------
--  awa-jobs -- AWA Jobs
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

with Ada.Finalization;

with Util.Beans.Objects;
with Util.Beans.Objects.Maps;

with ADO.Sessions;

with AWA.Events;
with AWA.Jobs.Models;

package AWA.Jobs.Services is

   Closed_Error   : exception;

   Schedule_Error : exception;

   Execute_Error  : exception;

   Invalid_Value  : exception;

   --  Event posted when a job is created.
   package Job_Create_Event is new AWA.Events.Definition (Name => "job-create");

   --  ------------------------------
   --  Abstract_Job Type
   --  ------------------------------
   --  The <b>Abstract_Job_Type</b> is an abstract tagged record which defines a job that can be
   --  scheduled and executed.
   type Abstract_Job_Type is abstract new Ada.Finalization.Limited_Controlled with private;
   type Abstract_Job_Access is access all Abstract_Job_Type'Class;

   --  Execute the job.  This operation must be implemented and should perform the work
   --  represented by the job.  It should use the <tt>Get_Parameter</tt> function to retrieve
   --  the job parameter and it can use the <tt>Set_Result</tt> operation to save the result.
   procedure Execute (Job : in out Abstract_Job_Type) is abstract;

   --  Set the job parameter identified by the <b>Name</b> to the value given in <b>Value</b>.
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in String);

   --  Set the job parameter identified by the <b>Name</b> to the value given in <b>Value</b>.
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in Integer);

   --  Set the job parameter identified by the <b>Name</b> to the value given in <b>Value</b>.
   --  The value object can hold any kind of basic value type (integer, enum, date, strings).
   --  If the value represents a bean, the <tt>Invalid_Value</tt> exception is raised.
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in Util.Beans.Objects.Object);

   --  Get the job parameter identified by the <b>Name</b> and convert the value into a string.
   function Get_Parameter (Job  : in Abstract_Job_Type;
                           Name : in String) return String;

   --  Get the job parameter identified by the <b>Name</b> and convert the value as an integer.
   --  If the parameter is not defined, return the default value passed in <b>Default</b>.
   function Get_Parameter (Job     : in Abstract_Job_Type;
                           Name    : in String;
                           Default : in Integer) return Integer;

   --  Get the job parameter identified by the <b>Name</b> and return it as a typed object.
   function Get_Parameter (Job  : in Abstract_Job_Type;
                           Name : in String) return Util.Beans.Objects.Object;

   --  Get the job status.
   function Get_Status (Job : in Abstract_Job_Type) return AWA.Jobs.Models.Job_Status_Type;

   --  Get the job identifier once the job was scheduled.  The job identifier allows to
   --  retrieve the job and check its execution and completion status later on.
   function Get_Identifier (Job : in Abstract_Job_Type) return ADO.Identifier;

   --  Set the job status.
   --  When the job is terminated, it is closed and the job parameters or results cannot be
   --  changed.
   procedure Set_Status (Job    : in out Abstract_Job_Type;
                         Status : in AWA.Jobs.Models.Job_Status_Type);

   --  Save the job information in the database.  Use the database session defined by <b>DB</b>
   --  to save the job.
   procedure Save (Job : in out Abstract_Job_Type;
                   DB  : in out ADO.Sessions.Master_Session'Class);

   --  ------------------------------
   --  Job Factory
   --  ------------------------------
   --  The <b>Job_Factory</b> is the interface that allows to create a job instance in order
   --  to execute a scheduled job.
   type Job_Factory is abstract tagged limited null record;
   type Job_Factory_Access is access all Job_Factory'Class;

   --  Create the job instance using the job factory.
   function Create (Factory : in Job_Factory) return Abstract_Job_Access is abstract;

   --  Get the job factory name.
   function Get_Name (Factory : in Job_Factory'Class) return String;

   --  Schedule the job.
   procedure Schedule (Job        : in out Abstract_Job_Type;
                       Definition : in Job_Factory'Class);

   --  ------------------------------
   --  Work Factory
   --  ------------------------------
   type Work_Access is access procedure (Job : in out Abstract_Job_Type'Class);

   type Work_Factory (Work : Work_Access) is new Job_Factory with null record;

   overriding
   function Create (Factory : in Work_Factory) return Abstract_Job_Access;

   --  ------------------------------
   --
   --  ------------------------------
   type Job_Type is new Abstract_Job_Type with private;

   procedure Set_Work (Job  : in out Job_Type;
                       Work : in Work_Factory'Class);

   procedure Execute (Job : in out Job_Type);

   --  ------------------------------
   --  Job Declaration
   --  ------------------------------
   --  The <tt>Definition</tt> package must be instantiated with a given job type to
   --  register the new job definition.
   generic
      type T is new Abstract_Job_Type with private;
   package Definition is
      type Job_Type_Factory is new Job_Factory with null record;

      overriding
      function Create (Factory : in Job_Type_Factory) return Abstract_Job_Access;

      Factory : aliased Job_Type_Factory;

   end Definition;

   generic
      Work : in Work_Access;
   package Work_Definition is
      type S_Factory is new Work_Factory with null record;

      Factory : aliased S_Factory := S_Factory '(Work => Work);
   end Work_Definition;

   --  Execute the job associated with the given event.
   procedure Execute (Event : in AWA.Events.Module_Event'Class);

private

   --  Execute the job and save the job information in the database.
   procedure Execute (Job : in out Abstract_Job_Type'Class;
                      DB  : in out ADO.Sessions.Master_Session'Class);

   type Abstract_Job_Type is abstract new Ada.Finalization.Limited_Controlled with record
      Job              : AWA.Jobs.Models.Job_Ref;
      Props            : Util.Beans.Objects.Maps.Map;
      Results          : Util.Beans.Objects.Maps.Map;
      Props_Modified   : Boolean := False;
      Results_Modified : Boolean := False;
   end record;

   type Job_Type is new Abstract_Job_Type with record
      Work : Work_Access;
   end record;

end AWA.Jobs.Services;
