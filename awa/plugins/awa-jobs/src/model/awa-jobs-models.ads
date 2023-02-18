-----------------------------------------------------------------------
--  AWA.Jobs.Models -- AWA.Jobs.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.4.0
-----------------------------------------------------------------------
--  Copyright (C) 2023 Stephane Carrez
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
pragma Warnings (Off);
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Objects.Enums;
with Util.Beans.Basic.Lists;
with AWA.Events.Models;
with AWA.Users.Models;
pragma Warnings (On);
package AWA.Jobs.Models is

   pragma Style_Checks ("-mrIu");

   type Job_Status_Type is (SCHEDULED, RUNNING, CANCELED, FAILED, TERMINATED);
   for Job_Status_Type use (SCHEDULED => 0, RUNNING => 1, CANCELED => 2, FAILED => 3, TERMINATED => 4);
   package Job_Status_Type_Objects is
      new Util.Beans.Objects.Enums (Job_Status_Type);

   type Nullable_Job_Status_Type is record
      Is_Null : Boolean := True;
      Value   : Job_Status_Type;
   end record;

   type Job_Ref is new ADO.Objects.Object_Ref with null record;

   --  --------------------
   --  The job is associated with a dispatching queue.
   --  --------------------
   --  Create an object key for Job.
   function Job_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Job from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Job_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Job : constant Job_Ref;
   function "=" (Left, Right : Job_Ref'Class) return Boolean;

   --  Set the job identifier
   procedure Set_Id (Object : in out Job_Ref;
                     Value  : in ADO.Identifier);

   --  Get the job identifier
   function Get_Id (Object : in Job_Ref)
                 return ADO.Identifier;

   --  Set the job status
   procedure Set_Status (Object : in out Job_Ref;
                         Value  : in Job_Status_Type);

   --  Get the job status
   function Get_Status (Object : in Job_Ref)
                 return Job_Status_Type;

   --  Set the job name
   procedure Set_Name (Object : in out Job_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out Job_Ref;
                       Value : in String);

   --  Get the job name
   function Get_Name (Object : in Job_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in Job_Ref)
                 return String;

   --  Set the job start date
   procedure Set_Start_Date (Object : in out Job_Ref;
                             Value  : in ADO.Nullable_Time);

   --  Get the job start date
   function Get_Start_Date (Object : in Job_Ref)
                 return ADO.Nullable_Time;

   --  Set the job creation date
   procedure Set_Create_Date (Object : in out Job_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the job creation date
   function Get_Create_Date (Object : in Job_Ref)
                 return Ada.Calendar.Time;

   --  Set the job finish date
   procedure Set_Finish_Date (Object : in out Job_Ref;
                              Value  : in ADO.Nullable_Time);

   --  Get the job finish date
   function Get_Finish_Date (Object : in Job_Ref)
                 return ADO.Nullable_Time;

   --  Set the job progress indicator
   procedure Set_Progress (Object : in out Job_Ref;
                           Value  : in Integer);

   --  Get the job progress indicator
   function Get_Progress (Object : in Job_Ref)
                 return Integer;

   --  Set the job parameters
   procedure Set_Parameters (Object : in out Job_Ref;
                             Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Parameters (Object : in out Job_Ref;
                             Value : in String);

   --  Get the job parameters
   function Get_Parameters (Object : in Job_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Parameters (Object : in Job_Ref)
                 return String;

   --  Set the job result
   procedure Set_Results (Object : in out Job_Ref;
                          Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Results (Object : in out Job_Ref;
                          Value : in String);

   --  Get the job result
   function Get_Results (Object : in Job_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Results (Object : in Job_Ref)
                 return String;
   --
   function Get_Version (Object : in Job_Ref)
                 return Integer;

   --  Set the job priority
   procedure Set_Priority (Object : in out Job_Ref;
                           Value  : in Integer);

   --  Get the job priority
   function Get_Priority (Object : in Job_Ref)
                 return Integer;

   --
   procedure Set_User (Object : in out Job_Ref;
                       Value  : in AWA.Users.Models.User_Ref'Class);

   --
   function Get_User (Object : in Job_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --
   procedure Set_Event (Object : in out Job_Ref;
                        Value  : in AWA.Events.Models.Message_Ref'Class);

   --
   function Get_Event (Object : in Job_Ref)
                 return AWA.Events.Models.Message_Ref'Class;

   --
   procedure Set_Session (Object : in out Job_Ref;
                          Value  : in AWA.Users.Models.Session_Ref'Class);

   --
   function Get_Session (Object : in Job_Ref)
                 return AWA.Users.Models.Session_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Job_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Job_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Reload from the database the same object if it was modified.
   --  Returns True in `Updated` if the object was reloaded.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Reload (Object  : in out Job_Ref;
                     Session : in out ADO.Sessions.Session'Class;
                     Updated : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Job_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Job_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Job_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Job_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   JOB_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Job_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Job_Ref;
                   Into   : in out Job_Ref);




private
   JOB_NAME : aliased constant String := "awa_job";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "status";
   COL_2_1_NAME : aliased constant String := "name";
   COL_3_1_NAME : aliased constant String := "start_date";
   COL_4_1_NAME : aliased constant String := "create_date";
   COL_5_1_NAME : aliased constant String := "finish_date";
   COL_6_1_NAME : aliased constant String := "progress";
   COL_7_1_NAME : aliased constant String := "parameters";
   COL_8_1_NAME : aliased constant String := "results";
   COL_9_1_NAME : aliased constant String := "version";
   COL_10_1_NAME : aliased constant String := "priority";
   COL_11_1_NAME : aliased constant String := "user_id";
   COL_12_1_NAME : aliased constant String := "event_id";
   COL_13_1_NAME : aliased constant String := "session_id";

   JOB_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 14,
      Table   => JOB_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access,
         6 => COL_5_1_NAME'Access,
         7 => COL_6_1_NAME'Access,
         8 => COL_7_1_NAME'Access,
         9 => COL_8_1_NAME'Access,
         10 => COL_9_1_NAME'Access,
         11 => COL_10_1_NAME'Access,
         12 => COL_11_1_NAME'Access,
         13 => COL_12_1_NAME'Access,
         14 => COL_13_1_NAME'Access)
     );
   JOB_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := JOB_DEF'Access;


   Null_Job : constant Job_Ref
      := Job_Ref'(ADO.Objects.Object_Ref with null record);

   type Job_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => JOB_DEF'Access)
   with record
       Status : Job_Status_Type;
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Start_Date : ADO.Nullable_Time;
       Create_Date : Ada.Calendar.Time;
       Finish_Date : ADO.Nullable_Time;
       Progress : Integer;
       Parameters : Ada.Strings.Unbounded.Unbounded_String;
       Results : Ada.Strings.Unbounded.Unbounded_String;
       Version : Integer;
       Priority : Integer;
       User : AWA.Users.Models.User_Ref;
       Event : AWA.Events.Models.Message_Ref;
       Session : AWA.Users.Models.Session_Ref;
   end record;

   type Job_Access is access all Job_Impl;

   overriding
   procedure Destroy (Object : access Job_Impl);

   overriding
   procedure Find (Object  : in out Job_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Job_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Job_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Job_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Job_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Job_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Job_Ref'Class;
                        Impl   : out Job_Access);
end AWA.Jobs.Models;
