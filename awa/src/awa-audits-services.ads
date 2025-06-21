-----------------------------------------------------------------------
--  awa-audits-services -- AWA Audit Manager
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Containers.Indefinite_Hashed_Maps;

with ADO.Audits;
with ADO.Sessions;
limited with AWA.Applications;
package AWA.Audits.Services is

   type Application_Access is access all AWA.Applications.Application'Class;

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
                             Entity  : in ADO.Entity_Type) return Integer;

   --  Initialize the audit manager.
   procedure Initialize (Manager : in out Audit_Manager;
                         App     : in Application_Access);

private

   type Field_Key (Len : Natural) is record
      Entity : ADO.Entity_Type;
      Name   : String (1 .. Len);
   end record;

   function Hash (Item : in Field_Key) return Ada.Containers.Hash_Type;

   package Audit_Field_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => Field_Key,
                                                Element_Type    => Integer,
                                                Hash            => Hash,
                                                Equivalent_Keys => "=");

   type Audit_Manager is limited new ADO.Audits.Audit_Manager with record
      Fields : Audit_Field_Maps.Map;
   end record;

end AWA.Audits.Services;
