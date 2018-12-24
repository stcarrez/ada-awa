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

with ADO.Audits;
with ADO.Sessions;

package AWA.Audits.Services is

   --  ------------------------------
   --  Event manager
   --  ------------------------------
   --  The <b>Event_Manager</b> manages the dispatch of event to the right event queue
   --  or to the event action.  The event manager holds a list of actions that must be
   --  triggered for a particular event/queue pair.  Such list is created and initialized
   --  when the application is configured.  It never changes.
   type Audit_Manager is limited new ADO.Audits.Audit_Manager with null record;
   type Audit_Manager_Access is access all Audit_Manager'Class;

   --  Save the audit changes in the database.
   overriding
   procedure Save (Manager : in out Audit_Manager;
                   Session : in out ADO.Sessions.Master_Session'Class;
                   Object  : in ADO.Audits.Auditable_Object_Record'Class;
                   Changes : in ADO.Audits.Audit_Array);

end AWA.Audits.Services;
