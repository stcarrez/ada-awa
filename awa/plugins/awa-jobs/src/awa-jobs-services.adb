-----------------------------------------------------------------------
--  awa-jobs-services -- Job services
--  Copyright (C) 2012, 2014, 2015, 2016, 2019, 2020, 2022 Stephane Carrez
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

with Ada.Tags;
with Ada.Calendar;

with ADO.Utils;
with ADO.Statements;

with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Jobs.Modules;
with AWA.Applications;
with AWA.Modules;
package body AWA.Jobs.Services is

   package ASC renames AWA.Services.Contexts;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Jobs.Services");

   --  ------------------------------
   --  Get the job status.
   --  ------------------------------
   function Get_Job_Status (Id : in ADO.Identifier) return Models.Job_Status_Type is
      Ctx  : constant ASC.Service_Context_Access := ASC.Current;
      DB   : constant ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Stmt : ADO.Statements.Query_Statement
        := DB.Create_Statement ("SELECT status FROM awa_job WHERE id = ?");
   begin
      Stmt.Add_Param (Id);
      Stmt.Execute;
      return Models.Job_Status_Type'Val (Stmt.Get_Result_Integer);
   end Get_Job_Status;

   --  ------------------------------
   --  Set the job parameter identified by the `Name` to the value
   --  given in `Value`.
   --  ------------------------------
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in String) is
   begin
      Job.Set_Parameter (Name, Util.Beans.Objects.To_Object (Value));
   end Set_Parameter;

   --  ------------------------------
   --  Set the job parameter identified by the `Name` to the value
   --  given in `Value`.
   --  ------------------------------
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in Integer) is
   begin
      Job.Set_Parameter (Name, Util.Beans.Objects.To_Object (Value));
   end Set_Parameter;

   --  ------------------------------
   --  Set the job parameter identified by the `Name` to the value
   --  given in `Value`.
   --  ------------------------------
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in ADO.Objects.Object_Ref'Class) is
   begin
      if Value.Is_Null then
         Job.Set_Parameter (Name, Util.Beans.Objects.Null_Object);
      else
         Job.Set_Parameter (Name, ADO.Objects.To_Object (Value.Get_Key));
      end if;
   end Set_Parameter;

   --  ------------------------------
   --  Set the job parameter identified by the `Name` to the value
   --  given in `Value`.
   --  The value object can hold any kind of basic value type
   --  (integer, enum, date, strings).  If the value represents
   --  a bean, the `Invalid_Value` exception is raised.
   --  ------------------------------
   procedure Set_Parameter (Job   : in out Abstract_Job_Type;
                            Name  : in String;
                            Value : in Util.Beans.Objects.Object) is
   begin
      Job.Props.Include (Name, Value);
      Job.Props_Modified := True;
   end Set_Parameter;

   --  ------------------------------
   --  Set the job result identified by the `Name` to the value given
   --  in `Value`.  The value object can hold any kind of basic value
   --  type (integer, enum, date, strings).  If the value represents a bean,
   --  the `Invalid_Value` exception is raised.
   --  ------------------------------
   procedure Set_Result (Job   : in out Abstract_Job_Type;
                         Name  : in String;
                         Value : in Util.Beans.Objects.Object) is
   begin
      Job.Results.Include (Name, Value);
      Job.Results_Modified := True;
   end Set_Result;

   --  ------------------------------
   --  Set the job result identified by the `Name` to the value given in `Value`.
   --  ------------------------------
   procedure Set_Result (Job   : in out Abstract_Job_Type;
                         Name  : in String;
                         Value : in String) is
   begin
      Job.Set_Result (Name, Util.Beans.Objects.To_Object (Value));
   end Set_Result;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (Job  : in Abstract_Job_Type;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return Job.Get_Parameter (Name);
   end Get_Value;

   --  ------------------------------
   --  Get the job parameter identified by the `Name` and convert
   --  the value into a string.
   --  ------------------------------
   function Get_Parameter (Job  : in Abstract_Job_Type;
                           Name : in String) return String is
      Value : constant Util.Beans.Objects.Object := Job.Get_Parameter (Name);
   begin
      return Util.Beans.Objects.To_String (Value);
   end Get_Parameter;

   --  ------------------------------
   --  Get the job parameter identified by the `Name` and convert
   --  the value as an integer.  If the parameter is not defined,
   --  return the default value passed in `Default`.
   --  ------------------------------
   function Get_Parameter (Job     : in Abstract_Job_Type;
                           Name    : in String;
                           Default : in Integer) return Integer is
      Pos : constant Util.Beans.Objects.Maps.Cursor := Job.Props.Find (Name);
   begin
      if Util.Beans.Objects.Maps.Has_Element (Pos) then
         declare
            Value : constant Util.Beans.Objects.Object := Util.Beans.Objects.Maps.Element (Pos);
         begin
            if Util.Beans.Objects.Is_Null (Value) then
               return Default;
            else
               return Util.Beans.Objects.To_Integer (Value);
            end if;
         end;
      else
         return Default;
      end if;
   end Get_Parameter;

   --  ------------------------------
   --  Get the job parameter identified by the `Name` and convert
   --  the value as a database identifier.  If the parameter is not defined,
   --  return the `ADO.NO_IDENTIFIER`.
   --  ------------------------------
   function Get_Parameter (Job     : in Abstract_Job_Type;
                           Name    : in String) return ADO.Identifier is
      Pos : constant Util.Beans.Objects.Maps.Cursor := Job.Props.Find (Name);
   begin
      if Util.Beans.Objects.Maps.Has_Element (Pos) then
         declare
            Value : constant Util.Beans.Objects.Object := Util.Beans.Objects.Maps.Element (Pos);
         begin
            if Util.Beans.Objects.Is_Null (Value) then
               return ADO.NO_IDENTIFIER;
            else
               return ADO.Utils.To_Identifier (Value);
            end if;
         end;
      else
         return ADO.NO_IDENTIFIER;
      end if;
   end Get_Parameter;

   --  ------------------------------
   --  Get the job parameter identified by the `Name` and return it as
   --  a typed object.
   --  ------------------------------
   function Get_Parameter (Job  : in Abstract_Job_Type;
                           Name : in String) return Util.Beans.Objects.Object is
   begin
      return Job.Props.Element (Name);
   end Get_Parameter;

   --  ------------------------------
   --  Get the job status.
   --  ------------------------------
   function Get_Status (Job : in Abstract_Job_Type) return Models.Job_Status_Type is
   begin
      return Job.Job.Get_Status;
   end Get_Status;

   --  ------------------------------
   --  Get the job identifier once the job was scheduled.
   --  The job identifier allows to retrieve the job and check its
   --  execution and completion status later on.
   --  ------------------------------
   function Get_Identifier (Job : in Abstract_Job_Type) return ADO.Identifier is
   begin
      return Job.Job.Get_Id;
   end Get_Identifier;

   --  ------------------------------
   --  Set the job status.  When the job is terminated, it is closed
   --  and the job parameters or results cannot be changed.
   --  ------------------------------
   procedure Set_Status (Job    : in out Abstract_Job_Type;
                         Status : in AWA.Jobs.Models.Job_Status_Type) is
   begin
      case Job.Job.Get_Status is
         when AWA.Jobs.Models.CANCELED | Models.FAILED | Models.TERMINATED =>
            Log.Info ("Job {0} is closed", ADO.Identifier'Image (Job.Job.Get_Id));
            raise Closed_Error;

         when Models.SCHEDULED | Models.RUNNING =>
            Job.Job.Set_Status (Status);

      end case;
   end Set_Status;

   --  ------------------------------
   --  Save the job information in the database.  Use the database session
   --  defined by `DB` to save the job.
   --  ------------------------------
   procedure Save (Job : in out Abstract_Job_Type;
                   DB  : in out ADO.Sessions.Master_Session'Class) is
   begin
      if Job.Props_Modified then
         Job.Job.Set_Parameters (Util.Serialize.Tools.To_JSON (Job.Props));
         Job.Props_Modified := False;
      end if;
      if Job.Results_Modified then
         Job.Job.Set_Results (Util.Serialize.Tools.To_JSON (Job.Results));
         Job.Results_Modified := False;
      end if;
      Job.Job.Save (DB);
   end Save;

   --  ------------------------------
   --  Schedule the job.
   --  ------------------------------
   procedure Schedule (Job        : in out Abstract_Job_Type;
                       Definition : in Job_Factory'Class) is

      Ctx : constant ASC.Service_Context_Access := ASC.Current;
      DB   : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      User : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
      Sess : constant AWA.Users.Models.Session_Ref := Ctx.Get_User_Session;
      App  : constant AWA.Applications.Application_Access := Ctx.Get_Application;
      Msg  : AWA.Events.Module_Event;

   begin
      if Job.Job.Is_Inserted then
         Log.Error ("Job is already scheduled");
         raise Schedule_Error with "The job is already scheduled.";
      end if;
      Job.Job.Set_Create_Date (Ada.Calendar.Clock);

      DB.Begin_Transaction;
      Job.Job.Set_Name (Definition.Get_Name);
      Job.Job.Set_User (User);
      Job.Job.Set_Session (Sess);
      Job.Save (DB);

      --  Create the event
      Msg.Set_Parameters (Job.Props);
      Msg.Set_Entity (Job.Job, DB);
      Msg.Set_Event_Kind (Job_Create_Event.Kind);
      App.Send_Event (Msg);
      DB.Commit;
   end Schedule;

   --  ------------------------------
   --  Execute the job and save the job information in the database.
   --  ------------------------------
   procedure Execute (Job : in out Abstract_Job_Type'Class;
                      DB  : in out ADO.Sessions.Master_Session'Class) is
      use type AWA.Jobs.Models.Job_Status_Type;
   begin
      --  Execute the job with an exception guard.
      Log.Info ("Execute job {0}", String '(Job.Job.Get_Name));
      begin
         Job.Execute;

      exception
         when E : others =>
            Log.Error ("Exception when executing job {0}", Job.Job.Get_Name);
            Log.Error ("Exception:", E, True);
            Job.Job.Set_Status (Models.FAILED);
      end;

      --  If the job did not set a completion status, mark it as terminated.
      if Job.Job.Get_Status in Models.SCHEDULED | Models.RUNNING then
         Job.Job.Set_Status (Models.TERMINATED);
      end if;

      --  And save the job.
      DB.Begin_Transaction;
      Job.Job.Set_Finish_Date (ADO.Nullable_Time '(Is_Null => False,
                                                   Value   => Ada.Calendar.Clock));
      Job.Save (DB);
      DB.Commit;
   end Execute;

   --  ------------------------------
   --  Execute the job associated with the given event.
   --  ------------------------------
   procedure Execute (Event  : in AWA.Events.Module_Event'Class;
                      Result : in out Job_Ref) is
      use AWA.Jobs.Modules;
      use type AWA.Modules.Module_Access;

      Ctx    : constant ASC.Service_Context_Access := ASC.Current;
      App    : constant AWA.Applications.Application_Access := Ctx.Get_Application;
      Module : constant AWA.Modules.Module_Access := App.Find_Module (AWA.Jobs.Modules.NAME);
      DB     : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      Job    : AWA.Jobs.Models.Job_Ref;
      Id     : constant ADO.Identifier := Event.Get_Entity_Identifier;
   begin
      if Module = null then
         Log.Warn ("There is no Job module to execute a job");
         raise Execute_Error;
      end if;
      if not (Module.all in AWA.Jobs.Modules.Job_Module'Class) then
         Log.Warn ("The 'job' module is not a valid module for job execution");
         raise Execute_Error;
      end if;

      DB.Begin_Transaction;
      Job.Load (Session => DB,
                Id      => Id);
      Job.Set_Start_Date (ADO.Nullable_Time '(Is_Null => False,
                                              Value   => Ada.Calendar.Clock));
      Job.Set_Status (AWA.Jobs.Models.RUNNING);
      Job.Save (Session => DB);
      DB.Commit;

      declare
         Name  : constant String := Job.Get_Name;
         Ident : constant String := ADO.Identifier'Image (Id);
      begin
         Log.Info ("Restoring job {0} - '{1}'", Ident, Name);
         declare
            Plugin  : constant Job_Module_Access := Job_Module'Class (Module.all)'Access;
            Factory : constant Job_Factory_Access := Plugin.Find_Factory (Name);
            Work    : AWA.Jobs.Services.Abstract_Job_Type_Access := null;
         begin
            if Factory /= null then
               Work := Factory.Create;
               Work.Job := Job;
               Event.Copy (Work.Props);
               Result := Job_Ref '(Job_Refs.Create (Work) with null record);
               Work.Execute (DB);
            else
               Log.Error ("There is no factory to execute job {0} - '{1}'",
                          Ident, Name);
               Job.Set_Status (AWA.Jobs.Models.FAILED);
               Job.Set_Finish_Date (ADO.Nullable_Time '(Is_Null => False,
                                                        Value   => Ada.Calendar.Clock));
               DB.Begin_Transaction;
               Job.Save (Session => DB);
               DB.Commit;
            end if;
         end;
      end;
   end Execute;

   --  ------------------------------
   --  Get the job parameter identified by the `Name` and return it as
   --  a typed object.  Return the `Null_Object` if the job is empty
   --  or there is no such parameter.
   --  ------------------------------
   function Get_Parameter (Job  : in Job_Ref;
                           Name : in String) return Util.Beans.Objects.Object is
   begin
      if Job.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return Job.Value.Get_Parameter (Name);
      end if;
   end Get_Parameter;

   --  ------------------------------
   --  Get the job factory name.
   --  ------------------------------
   function Get_Name (Factory : in Job_Factory'Class) return String is
   begin
      return Ada.Tags.Expanded_Name (Factory'Tag);
   end Get_Name;

   overriding
   procedure Execute (Job : in out Job_Type) is
   begin
      Job.Work (Job);
   end Execute;

   --  ------------------------------
   --  Create the job instance to execute the associated `Work_Access` procedure.
   --  ------------------------------
   overriding
   function Create (Factory : in Work_Factory) return Abstract_Job_Type_Access is
   begin
      return new Job_Type '(Util.Refs.Ref_Entity with
                              Work => Factory.Work,
                              others => <>);
   end Create;

   --  ------------------------------
   --  Job Declaration
   --  ------------------------------
   package body Definition is

      overriding
      function Create (Factory : in Job_Type_Factory) return Abstract_Job_Type_Access is
         pragma Unreferenced (Factory);
      begin
         return new T;
      end Create;

   end Definition;

end AWA.Jobs.Services;
