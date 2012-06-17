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
with Util.Serialize.Tools;
with Util.Log.Loggers;

with Ada.Calendar;
with Ada.Tags;

with ADO.Sessions.Entities;

with AWA.Users.Models;
with AWA.Events.Models;
with AWA.Services.Contexts;
package body AWA.Jobs.Services is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Jobs.Services");

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
      Job.Props_Modified := True;
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

   --  ------------------------------
   --  Save the job information in the database.  Use the database session defined by <b>DB</b>
   --  to save the job.
   --  ------------------------------
   procedure Save (Job : in out Abstract_Job_Type;
                   DB  : in out ADO.Sessions.Master_Session'Class) is
   begin
      if Job.Results_Modified then
         Job.Job.Set_Results (Util.Serialize.Tools.To_JSON (Job.Results));
         Job.Results_Modified := False;
      end if;
      Job.Job.Save (DB);
   end Save;

   --  Schedule the job.
   procedure Schedule (Job    : in out Abstract_Job_Type) is
      Ctx : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB   : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Msg  : AWA.Events.Models.Message_Ref;
      User : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      Sess : constant AWA.Users.Models.Session_Ref := Ctx.Get_User_Session;
   begin
      if Job.Job.Is_Inserted then
         Log.Error ("Job is already scheduled");
         raise Schedule_Error with "The job is already scheduled.";
      end if;
      Job.Job.Set_Create_Date (Ada.Calendar.Clock);

      DB.Begin_Transaction;
      Job.Job.Set_User (User);
      Job.Job.Set_Session (Sess);
      Job.Save (DB);

      --  Create the event
--        Msg.Set_Message_Type
      Msg.Set_Parameters (Util.Serialize.Tools.To_JSON (Job.Props));
      Msg.Set_Create_Date (Job.Job.Get_Create_Date);
      Msg.Set_User (User);
      Msg.Set_Session (Sess);
      Msg.Set_Status (AWA.Events.Models.QUEUED);
      Msg.Set_Entity_Id (Job.Job.Get_Id);
      Msg.Set_Entity_Type (ADO.Sessions.Entities.Find_Entity_Type (Session => DB,
                                                                   Object  => Job.Job.Get_Key));
      Msg.Save (DB);

      Job.Job.Set_Event (Msg);
      Job.Job.Save (DB);
      DB.Commit;
   end Schedule;

   function Get_Name (Factory : in Job_Factory'Class) return String is
   begin
      return Ada.Tags.Expanded_Name (Factory'Tag);
   end Get_Name;

   procedure Set_Work (Job  : in out Job_Type;
                       Work : in Work_Factory'Class) is
   begin
      Job.Work := Work.Work;
      Job.Job.Set_Name (Work.Get_Name);
   end Set_Work;

   procedure Execute (Job : in out Job_Type) is
   begin
      Job.Work (Job);
   end Execute;

   overriding
   function Create (Factory : in Work_Factory) return Abstract_Job_Access is
   begin
      return new Job_Type '(Ada.Finalization.Limited_Controlled with
                              Work => Factory.Work,
                              others => <>);
   end Create;

   --  ------------------------------
   --  Job Declaration
   --  ------------------------------
   --  The <tt>Definition</tt> package must be instantiated with a given job type to
   --  register the new job definition.
   package body Definition is

      function Create (Factory : in Job_Type_Factory) return Abstract_Job_Access is
         pragma Unreferenced (Factory);
      begin
         return new T;
      end Create;

   end Definition;

end AWA.Jobs.Services;
