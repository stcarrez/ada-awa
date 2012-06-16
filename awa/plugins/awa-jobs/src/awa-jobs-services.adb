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

package body AWA.Jobs.Services is

   --  ------------------------------
   --  Job Type
   --  ------------------------------
   --  The <b>Job_Type</b> is an abstract tagged record which defines a job that can be
   --  scheduled and executed.

   --  ------------------------------
   --  Set the job parameter identified by the <b>Name</b> to the value given in <b>Value</b>.
   --  ------------------------------
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in String) is
   begin
      Job.Set_Parameter (Name, Util.Beans.Objects.To_Object (Value));
   end Set_Parameter;

   --  ------------------------------
   --  Set the job parameter identified by the <b>Name</b> to the value given in <b>Value</b>.
   --  The value object can hold any kind of basic value type (integer, enum, date, strings).
   --  If the value represents a bean, the <tt>Invalid_Value</tt> exception is raised.
   --  ------------------------------
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in Util.Beans.Objects.Object) is
   begin
      Job.Props.Include (Name, Value);
   end Set_Parameter;

   --  ------------------------------
   --  Get the job parameter identified by the <b>Name</b> and convert the value into a string.
   --  ------------------------------
   function Get_Parameter (Job  : in Abstract_Job_Type;
                           Name : in String) return String is
      Value : constant Util.Beans.Objects.Object := Job.Get_Parameter (Name);
   begin
      return Util.Beans.Objects.To_String (Value);
   end Get_Parameter;

   --  ------------------------------
   --  Get the job parameter identified by the <b>Name</b> and return it as a typed object.
   --  ------------------------------
   function Get_Parameter (Job  : in Abstract_Job_Type;
                           Name : in String) return Util.Beans.Objects.Object is
   begin
      return Job.Props.Element (Name);
   end Get_Parameter;

   --  ------------------------------
   --  Get the job status.
   --  ------------------------------
   function Get_Status (Job : in Abstract_Job_Type) return AWA.Jobs.Models.Job_Status_Type is
   begin
      return Job.Job.Get_Status;
   end Get_Status;

   --  ------------------------------
   --  Set the job status.
   --  When the job is terminated, it is closed and the job parameters or results cannot be
   --  changed.
   --  ------------------------------
   procedure Set_Status (Job    : in out Abstract_Job_Type;
                         Status : in AWA.Jobs.Models.Job_Status_Type) is
   begin
      case Job.Job.Get_Status is
         when AWA.Jobs.Models.CANCELED | Models.FAILED | Models.TERMINATED =>
            raise Closed_Error;

         when Models.SCHEDULED | Models.RUNNING =>
            Job.Job.Set_Status (Status);

      end case;
   end Set_Status;

   procedure Register (Name : in String;
                       Create : in Create_Job_Access) is
   begin
      null;
   end Register;

   procedure Set_Work (Job  : in out Job_Type;
                       Work : in Work_Access) is
   begin
      Job.Work := Work;
   end Set_Work;

   procedure Execute (Job : in out Job_Type) is
   begin
      Job.Work (Job);
   end Execute;

   --  ------------------------------
   --  Job Declaration
   --  ------------------------------
   --  The <tt>Definition</tt> package must be instantiated with a given job type to
   --  register the new job definition.
--     package body Definition is
--        function Create return Abstract_Job_Access is
--        begin
--           return new T;
--        end Create;
--
--  --     begin
--  --        Register (Ada.Tags.Expanded_Name (T'Tag), Create'Access);
--     end Definition;

end AWA.Jobs.Services;
