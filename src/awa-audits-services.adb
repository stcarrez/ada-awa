-----------------------------------------------------------------------
--  awa-audits-services -- AWA Audit Manager
--  Copyright (C) 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Calendar;
with Ada.Strings.Hash;

with Util.Strings;
with Util.Beans.Objects;
with Util.Log.Loggers;

with ADO.Objects;
with ADO.Schemas;
with ADO.Statements;
with ADO.Sessions.Entities;
with AWA.Audits.Models;
with AWA.Services.Contexts;
package body AWA.Audits.Services is

   package ASC renames AWA.Services.Contexts;
   package UBO renames Util.Beans.Objects;

   use type ASC.Service_Context_Access;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Audits.Services");

   function Hash (Item : in Field_Key) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Ada.Containers.Hash_Type (Item.Entity) + Ada.Strings.Hash (Item.Name);
   end Hash;

   --  ------------------------------
   --  Save the audit changes in the database.
   --  ------------------------------
   overriding
   procedure Save (Manager : in out Audit_Manager;
                   Session : in out ADO.Sessions.Master_Session'Class;
                   Object  : in ADO.Audits.Auditable_Object_Record'Class;
                   Changes : in ADO.Audits.Audit_Array) is
      Context : constant ASC.Service_Context_Access := ASC.Current;
      Now     : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Class   : constant ADO.Schemas.Class_Mapping_Access
        := Object.Get_Key.Of_Class;
      Kind    : constant ADO.Entity_Type
        := ADO.Sessions.Entities.Find_Entity_Type (Session, Object.Get_Key);
   begin
      for C of Changes loop
         declare
            Audit : AWA.Audits.Models.Audit_Ref;
            Field : constant Util.Strings.Name_Access := Class.Members (C.Field);
         begin
            Audit.Set_Entity_Id (ADO.Objects.Get_Value (Object.Get_Key));
            Audit.Set_Entity_Type (Kind);
            Audit.Set_Field (Manager.Get_Audit_Field (Field.all, Kind));
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

   --  ------------------------------
   --  Find the audit field identification number from the entity type and field name.
   --  ------------------------------
   function Get_Audit_Field (Manager : in Audit_Manager;
                             Name    : in String;
                             Entity  : in ADO.Entity_Type) return Integer is
      Key : constant Field_Key := Field_Key '(Len    => Name'Length,
                                              Name   => Name,
                                              Entity => Entity);
      Pos : constant Audit_Field_Maps.Cursor := Manager.Fields.Find (Key);
   begin
      if Audit_Field_Maps.Has_Element (Pos) then
         return Audit_Field_Maps.Element (Pos);
      else
         Log.Warn ("Audit field {0} for{1} not found",
                   Name, ADO.Entity_Type'Image (Entity));
         return 0;
      end if;
   end Get_Audit_Field;

   --  ------------------------------
   --  Initialize the audit manager.
   --  ------------------------------
   procedure Initialize (Manager : in out Audit_Manager;
                         App     : in Application_Access) is
      DB    : constant ADO.Sessions.Session := App.Get_Session;
      Stmt  : ADO.Statements.Query_Statement
        := DB.Create_Statement ("SELECT id, entity_type, name FROM awa_audit_field");
      Count : Natural := 0;
   begin
      Stmt.Execute;
      while Stmt.Has_Elements loop
         declare
            Id   : constant Integer := Stmt.Get_Integer (0);
            Kind : constant ADO.Entity_Type := ADO.Entity_Type (Stmt.Get_Integer (1));
            Name : constant String := Stmt.Get_String (2);
            Key  : constant Field_Key := (Len  => Name'Length,
                                          Name => Name,
                                          Entity => Kind);
         begin
            Log.Debug ("Field {0} of{1} ={2}",
                       Name, ADO.Entity_Type'Image (Kind), Integer'Image (Id));
            if Manager.Fields.Contains (Key) then
               Log.Error ("Field {0} of{1} is already defined",
                          Name, ADO.Entity_Type'Image (Kind));
            else
               Manager.Fields.Insert (Key => Key,
                                      New_Item => Id);
            end if;
         end;
         Count := Count + 1;
         Stmt.Next;
      end loop;
      Log.Info ("Loaded{0} audit fields", Natural'Image (Count));
   end Initialize;

end AWA.Audits.Services;
