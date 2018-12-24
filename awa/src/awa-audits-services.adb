-----------------------------------------------------------------------
--  awa-audits-services -- AWA Audit Manager
--  Copyright (C) 2018 Stephane Carrez
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

with Ada.Calendar;
with Util.Beans.Objects;
with ADO.Objects;
with ADO.Sessions.Entities;
with AWA.Audits.Models;
with AWA.Services.Contexts;
package body AWA.Audits.Services is

   package ASC renames AWA.Services.Contexts;
   package UBO renames Util.Beans.Objects;

   use type ASC.Service_Context_Access;

   --  ------------------------------
   --  Save the audit changes in the database.
   --  ------------------------------
   overriding
   procedure Save (Manager : in out Audit_Manager;
                   Session : in out ADO.Sessions.Master_Session'Class;
                   Object  : in ADO.Audits.Auditable_Object_Record'Class;
                   Changes : in ADO.Audits.Audit_Array) is
      pragma Unreferenced (Manager);

      Context : constant ASC.Service_Context_Access := ASC.Current;
      Now     : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Kind    : constant ADO.Entity_Type
        := ADO.Sessions.Entities.Find_Entity_Type (Session, Object.Get_Key);
   begin
      for C of Changes loop
         declare
            Audit : AWA.Audits.Models.Audit_Ref;
         begin
            Audit.Set_Entity_Id (ADO.Objects.Get_Value (Object.Get_Key));
            Audit.Set_Entity_Type (Kind);
            if UBO.Is_Null (C.Old_Value) then
               Audit.Set_Old_Value (ADO.Null_String);
            else
               Audit.Set_Old_Value (UBO.To_String (C.Old_Value));
            end if;
            if UBO.Is_Null (C.New_Value) then
               Audit.Set_New_Value (ADO.Null_String);
            else
               Audit.Set_New_Value (UBO.To_String (C.New_Value));
            end if;
            Audit.Set_Date (Now);
            if Context /= null then
               Audit.Set_Session (Context.Get_User_Session);
            end if;
            Audit.Save (Session);
         end;
      end loop;
   end Save;

end AWA.Audits.Services;
