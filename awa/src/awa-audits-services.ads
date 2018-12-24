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
with Ada.Containers.Indefinite_Hashed_Maps;

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
   type Audit_Manager is limited new ADO.Audits.Audit_Manager with private;
   type Audit_Manager_Access is access all Audit_Manager'Class;

   --  Save the audit changes in the database.
   overriding
   procedure Save (Manager : in out Audit_Manager;
                   Session : in out ADO.Sessions.Master_Session'Class;
                   Object  : in ADO.Audits.Auditable_Object_Record'Class;
                   Changes : in ADO.Audits.Audit_Array);

   --  Find the audit field identification number from the entity type and field name.
   function Get_Audit_Field (Manager : in Audit_Manager;
                             Name    : in String;
                             Entity  : in ADO.Entity_Type) return ADO.Identifier;

private

   type Field_Key (Len : Natural) is record
      Entity : ADO.Entity_Type;
      Name   : String (1 .. Len);
   end record;

   function Hash (Item : in Field_Key) return Ada.Containers.Hash_Type;

   package Audit_Field_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => Field_Key,
                                                Element_Type    => ADO.Identifier,
                                                Hash            => Hash,
                                                Equivalent_Keys => "=",
                                                "="             => ADO."=");

   type Audit_Manager is limited new ADO.Audits.Audit_Manager with record
      Fields : Audit_Field_Maps.Map;
   end record;

end AWA.Audits.Services;
