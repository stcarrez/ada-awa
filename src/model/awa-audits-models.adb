-----------------------------------------------------------------------
--  AWA.Audits.Models -- AWA.Audits.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-body.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.4.0
-----------------------------------------------------------------------
--  Copyright (C) 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
pragma Warnings (Off);
with Ada.Unchecked_Deallocation;
with Util.Beans.Objects.Time;
pragma Warnings (On);
package body AWA.Audits.Models is

   pragma Style_Checks ("-mrIu");
   pragma Warnings (Off, "formal parameter * is not referenced");
   pragma Warnings (Off, "use clause for type *");
   pragma Warnings (Off, "use clause for private type *");

   use type ADO.Objects.Object_Record_Access;
   use type ADO.Objects.Object_Ref;

   function Audit_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key is
      Result : ADO.Objects.Object_Key (Of_Type  => ADO.Objects.KEY_INTEGER,
                                       Of_Class => AUDIT_DEF'Access);
   begin
      ADO.Objects.Set_Value (Result, Id);
      return Result;
   end Audit_Key;

   function Audit_Key (Id : in String) return ADO.Objects.Object_Key is
      Result : ADO.Objects.Object_Key (Of_Type  => ADO.Objects.KEY_INTEGER,
                                       Of_Class => AUDIT_DEF'Access);
   begin
      ADO.Objects.Set_Value (Result, Id);
      return Result;
   end Audit_Key;

   function "=" (Left, Right : Audit_Ref'Class) return Boolean is
   begin
      return ADO.Objects.Object_Ref'Class (Left) = ADO.Objects.Object_Ref'Class (Right);
   end "=";

   procedure Set_Field (Object : in out Audit_Ref'Class;
                        Impl   : out Audit_Access) is
      Result : ADO.Objects.Object_Record_Access;
   begin
      Object.Prepare_Modify (Result);
      Impl := Audit_Impl (Result.all)'Access;
   end Set_Field;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Audit_Ref) is
      Impl : Audit_Access;
   begin
      Impl := new Audit_Impl;
      Impl.Date := ADO.DEFAULT_TIME;
      Impl.Old_Value.Is_Null := True;
      Impl.New_Value.Is_Null := True;
      Impl.Entity_Id := ADO.NO_IDENTIFIER;
      Impl.Field := 0;
      Impl.Entity_Type := 0;
      ADO.Objects.Set_Object (Object, Impl.all'Access);
   end Allocate;

   -- ----------------------------------------
   --  Data object: Audit
   -- ----------------------------------------

   procedure Set_Id (Object : in out Audit_Ref;
                     Value  : in ADO.Identifier) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Key_Value (Impl.all, 1, Value);
   end Set_Id;

   function Get_Id (Object : in Audit_Ref)
                  return ADO.Identifier is
      Impl : constant Audit_Access
         := Audit_Impl (Object.Get_Object.all)'Access;
   begin
      return Impl.Get_Key_Value;
   end Get_Id;


   procedure Set_Date (Object : in out Audit_Ref;
                       Value  : in Ada.Calendar.Time) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Time (Impl.all, 2, Impl.Date, Value);
   end Set_Date;

   function Get_Date (Object : in Audit_Ref)
                  return Ada.Calendar.Time is
      Impl : constant Audit_Access
         := Audit_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Date;
   end Get_Date;


   procedure Set_Old_Value (Object : in out Audit_Ref;
                             Value : in String) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_String (Impl.all, 3, Impl.Old_Value, Value);
   end Set_Old_Value;

   procedure Set_Old_Value (Object : in out Audit_Ref;
                            Value  : in ADO.Nullable_String) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_String (Impl.all, 3, Impl.Old_Value, Value);
   end Set_Old_Value;

   function Get_Old_Value (Object : in Audit_Ref)
                 return String is
      Value : constant ADO.Nullable_String := Object.Get_Old_Value;
   begin
      if Value.Is_Null then
         return "";
      else
         return Ada.Strings.Unbounded.To_String (Value.Value);
      end if;
   end Get_Old_Value;
   function Get_Old_Value (Object : in Audit_Ref)
                  return ADO.Nullable_String is
      Impl : constant Audit_Access
         := Audit_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Old_Value;
   end Get_Old_Value;


   procedure Set_New_Value (Object : in out Audit_Ref;
                             Value : in String) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_String (Impl.all, 4, Impl.New_Value, Value);
   end Set_New_Value;

   procedure Set_New_Value (Object : in out Audit_Ref;
                            Value  : in ADO.Nullable_String) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_String (Impl.all, 4, Impl.New_Value, Value);
   end Set_New_Value;

   function Get_New_Value (Object : in Audit_Ref)
                 return String is
      Value : constant ADO.Nullable_String := Object.Get_New_Value;
   begin
      if Value.Is_Null then
         return "";
      else
         return Ada.Strings.Unbounded.To_String (Value.Value);
      end if;
   end Get_New_Value;
   function Get_New_Value (Object : in Audit_Ref)
                  return ADO.Nullable_String is
      Impl : constant Audit_Access
         := Audit_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.New_Value;
   end Get_New_Value;


   procedure Set_Entity_Id (Object : in out Audit_Ref;
                            Value  : in ADO.Identifier) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Identifier (Impl.all, 5, Impl.Entity_Id, Value);
   end Set_Entity_Id;

   function Get_Entity_Id (Object : in Audit_Ref)
                  return ADO.Identifier is
      Impl : constant Audit_Access
         := Audit_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Entity_Id;
   end Get_Entity_Id;


   procedure Set_Field (Object : in out Audit_Ref;
                        Value  : in Integer) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Integer (Impl.all, 6, Impl.Field, Value);
   end Set_Field;

   function Get_Field (Object : in Audit_Ref)
                  return Integer is
      Impl : constant Audit_Access
         := Audit_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Field;
   end Get_Field;


   procedure Set_Session (Object : in out Audit_Ref;
                          Value  : in AWA.Users.Models.Session_Ref'Class) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Object (Impl.all, 7, Impl.Session, Value);
   end Set_Session;

   function Get_Session (Object : in Audit_Ref)
                  return AWA.Users.Models.Session_Ref'Class is
      Impl : constant Audit_Access
         := Audit_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Session;
   end Get_Session;


   procedure Set_Entity_Type (Object : in out Audit_Ref;
                              Value  : in ADO.Entity_Type) is
      Impl : Audit_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Entity_Type (Impl.all, 8, Impl.Entity_Type, Value);
   end Set_Entity_Type;

   function Get_Entity_Type (Object : in Audit_Ref)
                  return ADO.Entity_Type is
      Impl : constant Audit_Access
         := Audit_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Entity_Type;
   end Get_Entity_Type;

   --  Copy of the object.
   procedure Copy (Object : in Audit_Ref;
                   Into   : in out Audit_Ref) is
      Result : Audit_Ref;
   begin
      if not Object.Is_Null then
         declare
            Impl : constant Audit_Access
              := Audit_Impl (Object.Get_Load_Object.all)'Access;
            Copy : constant Audit_Access
              := new Audit_Impl;
         begin
            ADO.Objects.Set_Object (Result, Copy.all'Access);
            Copy.Copy (Impl.all);
            Copy.Date := Impl.Date;
            Copy.Old_Value := Impl.Old_Value;
            Copy.New_Value := Impl.New_Value;
            Copy.Entity_Id := Impl.Entity_Id;
            Copy.Field := Impl.Field;
            Copy.Session := Impl.Session;
            Copy.Entity_Type := Impl.Entity_Type;
         end;
      end if;
      Into := Result;
   end Copy;

   overriding
   procedure Find (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean) is
      Impl  : constant Audit_Access := new Audit_Impl;
   begin
      Impl.Find (Session, Query, Found);
      if Found then
         ADO.Objects.Set_Object (Object, Impl.all'Access);
      else
         ADO.Objects.Set_Object (Object, null);
         Destroy (Impl);
      end if;
   end Find;

   procedure Load (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier) is
      Impl  : constant Audit_Access := new Audit_Impl;
      Found : Boolean;
      Query : ADO.SQL.Query;
   begin
      Query.Bind_Param (Position => 1, Value => Id);
      Query.Set_Filter ("id = ?");
      Impl.Find (Session, Query, Found);
      if not Found then
         Destroy (Impl);
         raise ADO.Objects.NOT_FOUND;
      end if;
      ADO.Objects.Set_Object (Object, Impl.all'Access);
   end Load;

   procedure Load (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean) is
      Impl  : constant Audit_Access := new Audit_Impl;
      Query : ADO.SQL.Query;
   begin
      Query.Bind_Param (Position => 1, Value => Id);
      Query.Set_Filter ("id = ?");
      Impl.Find (Session, Query, Found);
      if not Found then
         Destroy (Impl);
      else
         ADO.Objects.Set_Object (Object, Impl.all'Access);
      end if;
   end Load;

   overriding
   procedure Save (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class) is
      Impl : ADO.Objects.Object_Record_Access := Object.Get_Object;
   begin
      if Impl = null then
         Impl := new Audit_Impl;
         ADO.Objects.Set_Object (Object, Impl);
      end if;
      if not ADO.Objects.Is_Created (Impl.all) then
         Impl.Create (Session);
      else
         Impl.Save (Session);
      end if;
   end Save;

   overriding
   procedure Delete (Object  : in out Audit_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class) is
      Impl : constant ADO.Objects.Object_Record_Access := Object.Get_Object;
   begin
      if Impl /= null then
         Impl.Delete (Session);
      end if;
   end Delete;

   --  --------------------
   --  Free the object
   --  --------------------
   overriding
   procedure Destroy (Object : access Audit_Impl) is
      type Audit_Impl_Ptr is access all Audit_Impl;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
              (Audit_Impl, Audit_Impl_Ptr);
      pragma Warnings (Off, "*redundant conversion*");
      Ptr : Audit_Impl_Ptr := Audit_Impl (Object.all)'Access;
      pragma Warnings (On, "*redundant conversion*");
   begin
      Unchecked_Free (Ptr);
   end Destroy;

   overriding
   procedure Find (Object  : in out Audit_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean) is
      Stmt : ADO.Statements.Query_Statement
          := Session.Create_Statement (Query, AUDIT_DEF'Access);
   begin
      Stmt.Execute;
      if Stmt.Has_Elements then
         Object.Load (Stmt, Session);
         Stmt.Next;
         Found := not Stmt.Has_Elements;
      else
         Found := False;
      end if;
   end Find;

   overriding
   procedure Load (Object  : in out Audit_Impl;
                   Session : in out ADO.Sessions.Session'Class) is
      Found : Boolean;
      Query : ADO.SQL.Query;
      Id    : constant ADO.Identifier := Object.Get_Key_Value;
   begin
      Query.Bind_Param (Position => 1, Value => Id);
      Query.Set_Filter ("id = ?");
      Object.Find (Session, Query, Found);
      if not Found then
         raise ADO.Objects.NOT_FOUND;
      end if;
   end Load;

   overriding
   procedure Save (Object  : in out Audit_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class) is
      Stmt : ADO.Statements.Update_Statement
         := Session.Create_Statement (AUDIT_DEF'Access);
   begin
      if Object.Is_Modified (6) then
         Stmt.Save_Field (Name  => COL_5_1_NAME, --  field
                          Value => Object.Field);
         Object.Clear_Modified (6);
      end if;
      if Object.Is_Modified (7) then
         Stmt.Save_Field (Name  => COL_6_1_NAME, --  session_id
                          Value => Object.Session);
         Object.Clear_Modified (7);
      end if;
      if Object.Is_Modified (8) then
         Stmt.Save_Field (Name  => COL_7_1_NAME, --  entity_type
                          Value => Object.Entity_Type);
         Object.Clear_Modified (8);
      end if;
      if Stmt.Has_Save_Fields then
         Stmt.Set_Filter (Filter => "id = ?");
         Stmt.Add_Param (Value => Object.Get_Key);
         declare
            Result : Integer;
         begin
            Stmt.Execute (Result);
            if Result /= 1 then
               if Result /= 0 then
                  raise ADO.Objects.UPDATE_ERROR;
               end if;
            end if;
         end;
      end if;
   end Save;

   overriding
   procedure Create (Object  : in out Audit_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class) is
      Query : ADO.Statements.Insert_Statement
                  := Session.Create_Statement (AUDIT_DEF'Access);
      Result : Integer;
   begin
      Session.Allocate (Id => Object);
      Query.Save_Field (Name  => COL_0_1_NAME, --  id
                        Value => Object.Get_Key);
      Query.Save_Field (Name  => COL_1_1_NAME, --  date
                        Value => Object.Date);
      Query.Save_Field (Name  => COL_2_1_NAME, --  old_value
                        Value => Object.Old_Value);
      Query.Save_Field (Name  => COL_3_1_NAME, --  new_value
                        Value => Object.New_Value);
      Query.Save_Field (Name  => COL_4_1_NAME, --  entity_id
                        Value => Object.Entity_Id);
      Query.Save_Field (Name  => COL_5_1_NAME, --  field
                        Value => Object.Field);
      Query.Save_Field (Name  => COL_6_1_NAME, --  session_id
                        Value => Object.Session);
      Query.Save_Field (Name  => COL_7_1_NAME, --  entity_type
                        Value => Object.Entity_Type);
      Query.Execute (Result);
      if Result /= 1 then
         raise ADO.Objects.INSERT_ERROR;
      end if;
      ADO.Objects.Set_Created (Object);
   end Create;

   overriding
   procedure Delete (Object  : in out Audit_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class) is
      Stmt : ADO.Statements.Delete_Statement
         := Session.Create_Statement (AUDIT_DEF'Access);
   begin
      Stmt.Set_Filter (Filter => "id = ?");
      Stmt.Add_Param (Value => Object.Get_Key);
      Stmt.Execute;
   end Delete;

   --  ------------------------------
   --  Get the bean attribute identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Audit_Ref;
                       Name : in String) return Util.Beans.Objects.Object is
      Obj  : ADO.Objects.Object_Record_Access;
      Impl : access Audit_Impl;
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      end if;
      Obj := From.Get_Load_Object;
      Impl := Audit_Impl (Obj.all)'Access;
      if Name = "id" then
         return ADO.Objects.To_Object (Impl.Get_Key);
      elsif Name = "date" then
         return Util.Beans.Objects.Time.To_Object (Impl.Date);
      elsif Name = "old_value" then
         if Impl.Old_Value.Is_Null then
            return Util.Beans.Objects.Null_Object;
         else
            return Util.Beans.Objects.To_Object (Impl.Old_Value.Value);
         end if;
      elsif Name = "new_value" then
         if Impl.New_Value.Is_Null then
            return Util.Beans.Objects.Null_Object;
         else
            return Util.Beans.Objects.To_Object (Impl.New_Value.Value);
         end if;
      elsif Name = "entity_id" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (Impl.Entity_Id));
      elsif Name = "field" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (Impl.Field));
      elsif Name = "entity_type" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (Impl.Entity_Type));
      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;


   procedure List (Object  : in out Audit_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class) is
      Stmt : ADO.Statements.Query_Statement
        := Session.Create_Statement (Query, AUDIT_DEF'Access);
   begin
      Stmt.Execute;
      Audit_Vectors.Clear (Object);
      while Stmt.Has_Elements loop
         declare
            Item : Audit_Ref;
            Impl : constant Audit_Access := new Audit_Impl;
         begin
            Impl.Load (Stmt, Session);
            ADO.Objects.Set_Object (Item, Impl.all'Access);
            Object.Append (Item);
         end;
         Stmt.Next;
      end loop;
   end List;

   --  ------------------------------
   --  Load the object from current iterator position
   --  ------------------------------
   procedure Load (Object  : in out Audit_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class) is
   begin
      Object.Set_Key_Value (Stmt.Get_Identifier (0));
      Object.Date := Stmt.Get_Time (1);
      Object.Old_Value := Stmt.Get_Nullable_String (2);
      Object.New_Value := Stmt.Get_Nullable_String (3);
      Object.Entity_Id := Stmt.Get_Identifier (4);
      Object.Field := Stmt.Get_Integer (5);
      if not Stmt.Is_Null (6) then
         Object.Session.Set_Key_Value (Stmt.Get_Identifier (6), Session);
      end if;
      Object.Entity_Type := ADO.Entity_Type (Stmt.Get_Integer (7));
      ADO.Objects.Set_Created (Object);
   end Load;
   function Audit_Field_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key is
      Result : ADO.Objects.Object_Key (Of_Type  => ADO.Objects.KEY_STRING,
                                       Of_Class => AUDIT_FIELD_DEF'Access);
   begin
      ADO.Objects.Set_Value (Result, Id);
      return Result;
   end Audit_Field_Key;

   function Audit_Field_Key (Id : in String) return ADO.Objects.Object_Key is
      Result : ADO.Objects.Object_Key (Of_Type  => ADO.Objects.KEY_STRING,
                                       Of_Class => AUDIT_FIELD_DEF'Access);
   begin
      ADO.Objects.Set_Value (Result, Id);
      return Result;
   end Audit_Field_Key;

   function "=" (Left, Right : Audit_Field_Ref'Class) return Boolean is
   begin
      return ADO.Objects.Object_Ref'Class (Left) = ADO.Objects.Object_Ref'Class (Right);
   end "=";

   procedure Set_Field (Object : in out Audit_Field_Ref'Class;
                        Impl   : out Audit_Field_Access) is
      Result : ADO.Objects.Object_Record_Access;
   begin
      Object.Prepare_Modify (Result);
      Impl := Audit_Field_Impl (Result.all)'Access;
   end Set_Field;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Audit_Field_Ref) is
      Impl : Audit_Field_Access;
   begin
      Impl := new Audit_Field_Impl;
      Impl.Entity_Type := 0;
      ADO.Objects.Set_Object (Object, Impl.all'Access);
   end Allocate;

   -- ----------------------------------------
   --  Data object: Audit_Field
   -- ----------------------------------------

   procedure Set_Id (Object : in out Audit_Field_Ref;
                     Value  : in Integer) is
      Impl : Audit_Field_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Key_Value (Impl.all, 1, ADO.Identifier (Value));
   end Set_Id;

   function Get_Id (Object : in Audit_Field_Ref)
                  return Integer is
      Impl : constant Audit_Field_Access
         := Audit_Field_Impl (Object.Get_Object.all)'Access;
   begin
      return Integer (ADO.Identifier '(Impl.Get_Key_Value));
   end Get_Id;


   procedure Set_Name (Object : in out Audit_Field_Ref;
                        Value : in String) is
      Impl : Audit_Field_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_String (Impl.all, 2, Impl.Name, Value);
   end Set_Name;

   procedure Set_Name (Object : in out Audit_Field_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String) is
      Impl : Audit_Field_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Unbounded_String (Impl.all, 2, Impl.Name, Value);
   end Set_Name;

   function Get_Name (Object : in Audit_Field_Ref)
                 return String is
   begin
      return Ada.Strings.Unbounded.To_String (Object.Get_Name);
   end Get_Name;
   function Get_Name (Object : in Audit_Field_Ref)
                  return Ada.Strings.Unbounded.Unbounded_String is
      Impl : constant Audit_Field_Access
         := Audit_Field_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Name;
   end Get_Name;


   procedure Set_Entity_Type (Object : in out Audit_Field_Ref;
                              Value  : in ADO.Entity_Type) is
      Impl : Audit_Field_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Entity_Type (Impl.all, 3, Impl.Entity_Type, Value);
   end Set_Entity_Type;

   function Get_Entity_Type (Object : in Audit_Field_Ref)
                  return ADO.Entity_Type is
      Impl : constant Audit_Field_Access
         := Audit_Field_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Entity_Type;
   end Get_Entity_Type;

   --  Copy of the object.
   procedure Copy (Object : in Audit_Field_Ref;
                   Into   : in out Audit_Field_Ref) is
      Result : Audit_Field_Ref;
   begin
      if not Object.Is_Null then
         declare
            Impl : constant Audit_Field_Access
              := Audit_Field_Impl (Object.Get_Load_Object.all)'Access;
            Copy : constant Audit_Field_Access
              := new Audit_Field_Impl;
         begin
            ADO.Objects.Set_Object (Result, Copy.all'Access);
            Copy.Copy (Impl.all);
            Copy.Name := Impl.Name;
            Copy.Entity_Type := Impl.Entity_Type;
         end;
      end if;
      Into := Result;
   end Copy;

   overriding
   procedure Find (Object  : in out Audit_Field_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean) is
      Impl  : constant Audit_Field_Access := new Audit_Field_Impl;
   begin
      Impl.Find (Session, Query, Found);
      if Found then
         ADO.Objects.Set_Object (Object, Impl.all'Access);
      else
         ADO.Objects.Set_Object (Object, null);
         Destroy (Impl);
      end if;
   end Find;

   procedure Load (Object  : in out Audit_Field_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in Integer) is
      Impl  : constant Audit_Field_Access := new Audit_Field_Impl;
      Found : Boolean;
      Query : ADO.SQL.Query;
   begin
      Query.Bind_Param (Position => 1, Value => Id);
      Query.Set_Filter ("id = ?");
      Impl.Find (Session, Query, Found);
      if not Found then
         Destroy (Impl);
         raise ADO.Objects.NOT_FOUND;
      end if;
      ADO.Objects.Set_Object (Object, Impl.all'Access);
   end Load;

   procedure Load (Object  : in out Audit_Field_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in Integer;
                   Found   : out Boolean) is
      Impl  : constant Audit_Field_Access := new Audit_Field_Impl;
      Query : ADO.SQL.Query;
   begin
      Query.Bind_Param (Position => 1, Value => Id);
      Query.Set_Filter ("id = ?");
      Impl.Find (Session, Query, Found);
      if not Found then
         Destroy (Impl);
      else
         ADO.Objects.Set_Object (Object, Impl.all'Access);
      end if;
   end Load;

   overriding
   procedure Save (Object  : in out Audit_Field_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class) is
      Impl : ADO.Objects.Object_Record_Access := Object.Get_Object;
   begin
      if Impl = null then
         Impl := new Audit_Field_Impl;
         ADO.Objects.Set_Object (Object, Impl);
      end if;
      if not ADO.Objects.Is_Created (Impl.all) then
         Impl.Create (Session);
      else
         Impl.Save (Session);
      end if;
   end Save;

   overriding
   procedure Delete (Object  : in out Audit_Field_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class) is
      Impl : constant ADO.Objects.Object_Record_Access := Object.Get_Object;
   begin
      if Impl /= null then
         Impl.Delete (Session);
      end if;
   end Delete;

   --  --------------------
   --  Free the object
   --  --------------------
   overriding
   procedure Destroy (Object : access Audit_Field_Impl) is
      type Audit_Field_Impl_Ptr is access all Audit_Field_Impl;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
              (Audit_Field_Impl, Audit_Field_Impl_Ptr);
      pragma Warnings (Off, "*redundant conversion*");
      Ptr : Audit_Field_Impl_Ptr := Audit_Field_Impl (Object.all)'Access;
      pragma Warnings (On, "*redundant conversion*");
   begin
      Unchecked_Free (Ptr);
   end Destroy;

   overriding
   procedure Find (Object  : in out Audit_Field_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean) is
      Stmt : ADO.Statements.Query_Statement
          := Session.Create_Statement (Query, AUDIT_FIELD_DEF'Access);
   begin
      Stmt.Execute;
      if Stmt.Has_Elements then
         Object.Load (Stmt, Session);
         Stmt.Next;
         Found := not Stmt.Has_Elements;
      else
         Found := False;
      end if;
   end Find;

   overriding
   procedure Load (Object  : in out Audit_Field_Impl;
                   Session : in out ADO.Sessions.Session'Class) is
      Found : Boolean;
      Query : ADO.SQL.Query;
      Id    : constant Integer := Integer (ADO.Identifier '(Object.Get_Key_Value));
   begin
      Query.Bind_Param (Position => 1, Value => Id);
      Query.Set_Filter ("id = ?");
      Object.Find (Session, Query, Found);
      if not Found then
         raise ADO.Objects.NOT_FOUND;
      end if;
   end Load;

   overriding
   procedure Save (Object  : in out Audit_Field_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class) is
      Stmt : ADO.Statements.Update_Statement
         := Session.Create_Statement (AUDIT_FIELD_DEF'Access);
   begin
      if Object.Is_Modified (1) then
         Stmt.Save_Field (Name  => COL_0_2_NAME, --  id
                          Value => Object.Get_Key);
         Object.Clear_Modified (1);
      end if;
      if Object.Is_Modified (2) then
         Stmt.Save_Field (Name  => COL_1_2_NAME, --  name
                          Value => Object.Name);
         Object.Clear_Modified (2);
      end if;
      if Object.Is_Modified (3) then
         Stmt.Save_Field (Name  => COL_2_2_NAME, --  entity_type
                          Value => Object.Entity_Type);
         Object.Clear_Modified (3);
      end if;
      if Stmt.Has_Save_Fields then
         Stmt.Set_Filter (Filter => "id = ?");
         Stmt.Add_Param (Value => Object.Get_Key);
         declare
            Result : Integer;
         begin
            Stmt.Execute (Result);
            if Result /= 1 then
               if Result /= 0 then
                  raise ADO.Objects.UPDATE_ERROR;
               end if;
            end if;
         end;
      end if;
   end Save;

   overriding
   procedure Create (Object  : in out Audit_Field_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class) is
      Query : ADO.Statements.Insert_Statement
                  := Session.Create_Statement (AUDIT_FIELD_DEF'Access);
      Result : Integer;
   begin
      Session.Allocate (Id => Object);
      Query.Save_Field (Name  => COL_0_2_NAME, --  id
                        Value => Object.Get_Key);
      Query.Save_Field (Name  => COL_1_2_NAME, --  name
                        Value => Object.Name);
      Query.Save_Field (Name  => COL_2_2_NAME, --  entity_type
                        Value => Object.Entity_Type);
      Query.Execute (Result);
      if Result /= 1 then
         raise ADO.Objects.INSERT_ERROR;
      end if;
      ADO.Objects.Set_Created (Object);
   end Create;

   overriding
   procedure Delete (Object  : in out Audit_Field_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class) is
      Stmt : ADO.Statements.Delete_Statement
         := Session.Create_Statement (AUDIT_FIELD_DEF'Access);
   begin
      Stmt.Set_Filter (Filter => "id = ?");
      Stmt.Add_Param (Value => Object.Get_Key);
      Stmt.Execute;
   end Delete;

   --  ------------------------------
   --  Get the bean attribute identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Audit_Field_Ref;
                       Name : in String) return Util.Beans.Objects.Object is
      Obj  : ADO.Objects.Object_Record_Access;
      Impl : access Audit_Field_Impl;
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      end if;
      Obj := From.Get_Load_Object;
      Impl := Audit_Field_Impl (Obj.all)'Access;
      if Name = "id" then
         return ADO.Objects.To_Object (Impl.Get_Key);
      elsif Name = "name" then
         return Util.Beans.Objects.To_Object (Impl.Name);
      elsif Name = "entity_type" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (Impl.Entity_Type));
      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;



   --  ------------------------------
   --  Load the object from current iterator position
   --  ------------------------------
   procedure Load (Object  : in out Audit_Field_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class) is
   begin
      Object.Set_Key_Value (Stmt.Get_Unbounded_String (0));
      Object.Name := Stmt.Get_Unbounded_String (1);
      Object.Entity_Type := ADO.Entity_Type (Stmt.Get_Integer (2));
      ADO.Objects.Set_Created (Object);
   end Load;


end AWA.Audits.Models;
