-----------------------------------------------------------------------
--  AWA.Images.Models -- AWA.Images.Models
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
with ASF.Events.Faces.Actions;
pragma Warnings (On);
package body AWA.Images.Models is

   pragma Style_Checks ("-mrIu");
   pragma Warnings (Off, "formal parameter * is not referenced");
   pragma Warnings (Off, "use clause for type *");
   pragma Warnings (Off, "use clause for private type *");

   use type ADO.Objects.Object_Record_Access;
   use type ADO.Objects.Object_Ref;

   function Image_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key is
      Result : ADO.Objects.Object_Key (Of_Type  => ADO.Objects.KEY_INTEGER,
                                       Of_Class => IMAGE_DEF'Access);
   begin
      ADO.Objects.Set_Value (Result, Id);
      return Result;
   end Image_Key;

   function Image_Key (Id : in String) return ADO.Objects.Object_Key is
      Result : ADO.Objects.Object_Key (Of_Type  => ADO.Objects.KEY_INTEGER,
                                       Of_Class => IMAGE_DEF'Access);
   begin
      ADO.Objects.Set_Value (Result, Id);
      return Result;
   end Image_Key;

   function "=" (Left, Right : Image_Ref'Class) return Boolean is
   begin
      return ADO.Objects.Object_Ref'Class (Left) = ADO.Objects.Object_Ref'Class (Right);
   end "=";

   procedure Set_Field (Object : in out Image_Ref'Class;
                        Impl   : out Image_Access) is
      Result : ADO.Objects.Object_Record_Access;
   begin
      Object.Prepare_Modify (Result);
      Impl := Image_Impl (Result.all)'Access;
   end Set_Field;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Image_Ref) is
      Impl : Image_Access;
   begin
      Impl := new Image_Impl;
      Impl.Width := 0;
      Impl.Height := 0;
      Impl.Thumb_Width := 0;
      Impl.Thumb_Height := 0;
      Impl.Public := False;
      Impl.Version := 0;
      ADO.Objects.Set_Object (Object, Impl.all'Access);
   end Allocate;

   -- ----------------------------------------
   --  Data object: Image
   -- ----------------------------------------

   procedure Set_Id (Object : in out Image_Ref;
                     Value  : in ADO.Identifier) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Key_Value (Impl.all, 1, Value);
   end Set_Id;

   function Get_Id (Object : in Image_Ref)
                  return ADO.Identifier is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Object.all)'Access;
   begin
      return Impl.Get_Key_Value;
   end Get_Id;


   procedure Set_Width (Object : in out Image_Ref;
                        Value  : in Integer) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Integer (Impl.all, 2, Impl.Width, Value);
   end Set_Width;

   function Get_Width (Object : in Image_Ref)
                  return Integer is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Width;
   end Get_Width;


   procedure Set_Height (Object : in out Image_Ref;
                         Value  : in Integer) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Integer (Impl.all, 3, Impl.Height, Value);
   end Set_Height;

   function Get_Height (Object : in Image_Ref)
                  return Integer is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Height;
   end Get_Height;


   procedure Set_Thumb_Width (Object : in out Image_Ref;
                              Value  : in Integer) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Integer (Impl.all, 4, Impl.Thumb_Width, Value);
   end Set_Thumb_Width;

   function Get_Thumb_Width (Object : in Image_Ref)
                  return Integer is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Thumb_Width;
   end Get_Thumb_Width;


   procedure Set_Thumb_Height (Object : in out Image_Ref;
                               Value  : in Integer) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Integer (Impl.all, 5, Impl.Thumb_Height, Value);
   end Set_Thumb_Height;

   function Get_Thumb_Height (Object : in Image_Ref)
                  return Integer is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Thumb_Height;
   end Get_Thumb_Height;


   procedure Set_Path (Object : in out Image_Ref;
                        Value : in String) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_String (Impl.all, 6, Impl.Path, Value);
   end Set_Path;

   procedure Set_Path (Object : in out Image_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Unbounded_String (Impl.all, 6, Impl.Path, Value);
   end Set_Path;

   function Get_Path (Object : in Image_Ref)
                 return String is
   begin
      return Ada.Strings.Unbounded.To_String (Object.Get_Path);
   end Get_Path;
   function Get_Path (Object : in Image_Ref)
                  return Ada.Strings.Unbounded.Unbounded_String is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Path;
   end Get_Path;


   procedure Set_Public (Object : in out Image_Ref;
                         Value  : in Boolean) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Boolean (Impl.all, 7, Impl.Public, Value);
   end Set_Public;

   function Get_Public (Object : in Image_Ref)
                  return Boolean is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Public;
   end Get_Public;


   function Get_Version (Object : in Image_Ref)
                  return Integer is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Version;
   end Get_Version;


   procedure Set_Thumbnail (Object : in out Image_Ref;
                            Value  : in AWA.Storages.Models.Storage_Ref'Class) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Object (Impl.all, 9, Impl.Thumbnail, Value);
   end Set_Thumbnail;

   function Get_Thumbnail (Object : in Image_Ref)
                  return AWA.Storages.Models.Storage_Ref'Class is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Thumbnail;
   end Get_Thumbnail;


   procedure Set_Folder (Object : in out Image_Ref;
                         Value  : in AWA.Storages.Models.Storage_Folder_Ref'Class) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Object (Impl.all, 10, Impl.Folder, Value);
   end Set_Folder;

   function Get_Folder (Object : in Image_Ref)
                  return AWA.Storages.Models.Storage_Folder_Ref'Class is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Folder;
   end Get_Folder;


   procedure Set_Owner (Object : in out Image_Ref;
                        Value  : in AWA.Users.Models.User_Ref'Class) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Object (Impl.all, 11, Impl.Owner, Value);
   end Set_Owner;

   function Get_Owner (Object : in Image_Ref)
                  return AWA.Users.Models.User_Ref'Class is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Owner;
   end Get_Owner;


   procedure Set_Storage (Object : in out Image_Ref;
                          Value  : in AWA.Storages.Models.Storage_Ref'Class) is
      Impl : Image_Access;
   begin
      Set_Field (Object, Impl);
      ADO.Objects.Set_Field_Object (Impl.all, 12, Impl.Storage, Value);
   end Set_Storage;

   function Get_Storage (Object : in Image_Ref)
                  return AWA.Storages.Models.Storage_Ref'Class is
      Impl : constant Image_Access
         := Image_Impl (Object.Get_Load_Object.all)'Access;
   begin
      return Impl.Storage;
   end Get_Storage;

   --  Copy of the object.
   procedure Copy (Object : in Image_Ref;
                   Into   : in out Image_Ref) is
      Result : Image_Ref;
   begin
      if not Object.Is_Null then
         declare
            Impl : constant Image_Access
              := Image_Impl (Object.Get_Load_Object.all)'Access;
            Copy : constant Image_Access
              := new Image_Impl;
         begin
            ADO.Objects.Set_Object (Result, Copy.all'Access);
            Copy.Copy (Impl.all);
            Copy.Width := Impl.Width;
            Copy.Height := Impl.Height;
            Copy.Thumb_Width := Impl.Thumb_Width;
            Copy.Thumb_Height := Impl.Thumb_Height;
            Copy.Path := Impl.Path;
            Copy.Public := Impl.Public;
            Copy.Version := Impl.Version;
            Copy.Thumbnail := Impl.Thumbnail;
            Copy.Folder := Impl.Folder;
            Copy.Owner := Impl.Owner;
            Copy.Storage := Impl.Storage;
         end;
      end if;
      Into := Result;
   end Copy;

   overriding
   procedure Find (Object  : in out Image_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean) is
      Impl  : constant Image_Access := new Image_Impl;
   begin
      Impl.Find (Session, Query, Found);
      if Found then
         ADO.Objects.Set_Object (Object, Impl.all'Access);
      else
         ADO.Objects.Set_Object (Object, null);
         Destroy (Impl);
      end if;
   end Find;

   procedure Load (Object  : in out Image_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier) is
      Impl  : constant Image_Access := new Image_Impl;
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

   procedure Load (Object  : in out Image_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean) is
      Impl  : constant Image_Access := new Image_Impl;
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

   procedure Reload (Object  : in out Image_Ref;
                     Session : in out ADO.Sessions.Session'Class;
                     Updated : out Boolean) is
      Result : ADO.Objects.Object_Record_Access;
      Impl   : Image_Access;
      Query  : ADO.SQL.Query;
      Id     : ADO.Identifier;
   begin
      if Object.Is_Null then
         raise ADO.Objects.NULL_ERROR;
      end if;
      Object.Prepare_Modify (Result);
      Impl := Image_Impl (Result.all)'Access;
      Id := ADO.Objects.Get_Key_Value (Impl.all);
      Query.Bind_Param (Position => 1, Value => Id);
      Query.Bind_Param (Position => 2, Value => Impl.Version);
      Query.Set_Filter ("id = ? AND version != ?");
      declare
         Stmt : ADO.Statements.Query_Statement
             := Session.Create_Statement (Query, IMAGE_DEF'Access);
      begin
         Stmt.Execute;
         if Stmt.Has_Elements then
            Updated := True;
            Impl.Load (Stmt, Session);
         else
            Updated := False;
         end if;
      end;
   end Reload;

   overriding
   procedure Save (Object  : in out Image_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class) is
      Impl : ADO.Objects.Object_Record_Access := Object.Get_Object;
   begin
      if Impl = null then
         Impl := new Image_Impl;
         ADO.Objects.Set_Object (Object, Impl);
      end if;
      if not ADO.Objects.Is_Created (Impl.all) then
         Impl.Create (Session);
      else
         Impl.Save (Session);
      end if;
   end Save;

   overriding
   procedure Delete (Object  : in out Image_Ref;
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
   procedure Destroy (Object : access Image_Impl) is
      type Image_Impl_Ptr is access all Image_Impl;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
              (Image_Impl, Image_Impl_Ptr);
      pragma Warnings (Off, "*redundant conversion*");
      Ptr : Image_Impl_Ptr := Image_Impl (Object.all)'Access;
      pragma Warnings (On, "*redundant conversion*");
   begin
      Unchecked_Free (Ptr);
   end Destroy;

   overriding
   procedure Find (Object  : in out Image_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean) is
      Stmt : ADO.Statements.Query_Statement
          := Session.Create_Statement (Query, IMAGE_DEF'Access);
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
   procedure Load (Object  : in out Image_Impl;
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
   procedure Save (Object  : in out Image_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class) is
      Stmt : ADO.Statements.Update_Statement
         := Session.Create_Statement (IMAGE_DEF'Access);
   begin
      if Object.Is_Modified (1) then
         Stmt.Save_Field (Name  => COL_0_1_NAME, --  id
                          Value => Object.Get_Key);
         Object.Clear_Modified (1);
      end if;
      if Object.Is_Modified (2) then
         Stmt.Save_Field (Name  => COL_1_1_NAME, --  width
                          Value => Object.Width);
         Object.Clear_Modified (2);
      end if;
      if Object.Is_Modified (3) then
         Stmt.Save_Field (Name  => COL_2_1_NAME, --  height
                          Value => Object.Height);
         Object.Clear_Modified (3);
      end if;
      if Object.Is_Modified (4) then
         Stmt.Save_Field (Name  => COL_3_1_NAME, --  thumb_width
                          Value => Object.Thumb_Width);
         Object.Clear_Modified (4);
      end if;
      if Object.Is_Modified (5) then
         Stmt.Save_Field (Name  => COL_4_1_NAME, --  thumb_height
                          Value => Object.Thumb_Height);
         Object.Clear_Modified (5);
      end if;
      if Object.Is_Modified (6) then
         Stmt.Save_Field (Name  => COL_5_1_NAME, --  path
                          Value => Object.Path);
         Object.Clear_Modified (6);
      end if;
      if Object.Is_Modified (7) then
         Stmt.Save_Field (Name  => COL_6_1_NAME, --  public
                          Value => Object.Public);
         Object.Clear_Modified (7);
      end if;
      if Object.Is_Modified (9) then
         Stmt.Save_Field (Name  => COL_8_1_NAME, --  thumbnail_id
                          Value => Object.Thumbnail);
         Object.Clear_Modified (9);
      end if;
      if Object.Is_Modified (10) then
         Stmt.Save_Field (Name  => COL_9_1_NAME, --  folder_id
                          Value => Object.Folder);
         Object.Clear_Modified (10);
      end if;
      if Object.Is_Modified (11) then
         Stmt.Save_Field (Name  => COL_10_1_NAME, --  owner_id
                          Value => Object.Owner);
         Object.Clear_Modified (11);
      end if;
      if Object.Is_Modified (12) then
         Stmt.Save_Field (Name  => COL_11_1_NAME, --  storage_id
                          Value => Object.Storage);
         Object.Clear_Modified (12);
      end if;
      if Stmt.Has_Save_Fields then
         Object.Version := Object.Version + 1;
         Stmt.Save_Field (Name  => "version",
                          Value => Object.Version);
         Stmt.Set_Filter (Filter => "id = ? and version = ?");
         Stmt.Add_Param (Value => Object.Get_Key);
         Stmt.Add_Param (Value => Object.Version - 1);
         declare
            Result : Integer;
         begin
            Stmt.Execute (Result);
            if Result /= 1 then
               if Result /= 0 then
                  raise ADO.Objects.UPDATE_ERROR;
               else
                  raise ADO.Objects.LAZY_LOCK;
               end if;
            end if;
         end;
      end if;
   end Save;

   overriding
   procedure Create (Object  : in out Image_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class) is
      Query : ADO.Statements.Insert_Statement
                  := Session.Create_Statement (IMAGE_DEF'Access);
      Result : Integer;
   begin
      Object.Version := 1;
      Session.Allocate (Id => Object);
      Query.Save_Field (Name  => COL_0_1_NAME, --  id
                        Value => Object.Get_Key);
      Query.Save_Field (Name  => COL_1_1_NAME, --  width
                        Value => Object.Width);
      Query.Save_Field (Name  => COL_2_1_NAME, --  height
                        Value => Object.Height);
      Query.Save_Field (Name  => COL_3_1_NAME, --  thumb_width
                        Value => Object.Thumb_Width);
      Query.Save_Field (Name  => COL_4_1_NAME, --  thumb_height
                        Value => Object.Thumb_Height);
      Query.Save_Field (Name  => COL_5_1_NAME, --  path
                        Value => Object.Path);
      Query.Save_Field (Name  => COL_6_1_NAME, --  public
                        Value => Object.Public);
      Query.Save_Field (Name  => COL_7_1_NAME, --  version
                        Value => Object.Version);
      Query.Save_Field (Name  => COL_8_1_NAME, --  thumbnail_id
                        Value => Object.Thumbnail);
      Query.Save_Field (Name  => COL_9_1_NAME, --  folder_id
                        Value => Object.Folder);
      Query.Save_Field (Name  => COL_10_1_NAME, --  owner_id
                        Value => Object.Owner);
      Query.Save_Field (Name  => COL_11_1_NAME, --  storage_id
                        Value => Object.Storage);
      Query.Execute (Result);
      if Result /= 1 then
         raise ADO.Objects.INSERT_ERROR;
      end if;
      ADO.Objects.Set_Created (Object);
   end Create;

   overriding
   procedure Delete (Object  : in out Image_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class) is
      Stmt : ADO.Statements.Delete_Statement
         := Session.Create_Statement (IMAGE_DEF'Access);
   begin
      Stmt.Set_Filter (Filter => "id = ?");
      Stmt.Add_Param (Value => Object.Get_Key);
      Stmt.Execute;
   end Delete;

   --  ------------------------------
   --  Get the bean attribute identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Image_Ref;
                       Name : in String) return Util.Beans.Objects.Object is
      Obj  : ADO.Objects.Object_Record_Access;
      Impl : access Image_Impl;
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      end if;
      Obj := From.Get_Load_Object;
      Impl := Image_Impl (Obj.all)'Access;
      if Name = "id" then
         return ADO.Objects.To_Object (Impl.Get_Key);
      elsif Name = "width" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (Impl.Width));
      elsif Name = "height" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (Impl.Height));
      elsif Name = "thumb_width" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (Impl.Thumb_Width));
      elsif Name = "thumb_height" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (Impl.Thumb_Height));
      elsif Name = "path" then
         return Util.Beans.Objects.To_Object (Impl.Path);
      elsif Name = "public" then
         return Util.Beans.Objects.To_Object (Impl.Public);
      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;


   procedure List (Object  : in out Image_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class) is
      Stmt : ADO.Statements.Query_Statement
        := Session.Create_Statement (Query, IMAGE_DEF'Access);
   begin
      Stmt.Execute;
      Image_Vectors.Clear (Object);
      while Stmt.Has_Elements loop
         declare
            Item : Image_Ref;
            Impl : constant Image_Access := new Image_Impl;
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
   procedure Load (Object  : in out Image_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class) is
   begin
      Object.Set_Key_Value (Stmt.Get_Identifier (0));
      Object.Width := Stmt.Get_Integer (1);
      Object.Height := Stmt.Get_Integer (2);
      Object.Thumb_Width := Stmt.Get_Integer (3);
      Object.Thumb_Height := Stmt.Get_Integer (4);
      Object.Path := Stmt.Get_Unbounded_String (5);
      Object.Public := Stmt.Get_Boolean (6);
      if not Stmt.Is_Null (8) then
         Object.Thumbnail.Set_Key_Value (Stmt.Get_Identifier (8), Session);
      end if;
      if not Stmt.Is_Null (9) then
         Object.Folder.Set_Key_Value (Stmt.Get_Identifier (9), Session);
      end if;
      if not Stmt.Is_Null (10) then
         Object.Owner.Set_Key_Value (Stmt.Get_Identifier (10), Session);
      end if;
      if not Stmt.Is_Null (11) then
         Object.Storage.Set_Key_Value (Stmt.Get_Identifier (11), Session);
      end if;
      Object.Version := Stmt.Get_Integer (7);
      ADO.Objects.Set_Created (Object);
   end Load;
   procedure Op_Load (Bean    : in out Image_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String);
   procedure Op_Load (Bean    : in out Image_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Image_Bean'Class (Bean).Load (Outcome);
   end Op_Load;
   package Binding_Image_Bean_1 is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Image_Bean,
                                                      Method => Op_Load,
                                                      Name   => "load");

   Binding_Image_Bean_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Binding_Image_Bean_1.Proxy'Access
     );

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression.
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Image_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Binding_Image_Bean_Array'Access;
   end Get_Method_Bindings;
   --  ------------------------------
   --  Get the bean attribute identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Image_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "folder_id" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Folder_Id));
      elsif Name = "folder_name" then
         return Util.Beans.Objects.To_Object (From.Folder_Name);
      elsif Name = "id" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Id));
      elsif Name = "name" then
         return Util.Beans.Objects.To_Object (From.Name);
      elsif Name = "create_date" then
         return Util.Beans.Objects.Time.To_Object (From.Create_Date);
      elsif Name = "uri" then
         return Util.Beans.Objects.To_Object (From.Uri);
      elsif Name = "storage" then
         return AWA.Storages.Models.Storage_Type_Objects.To_Object (From.Storage);
      elsif Name = "mime_type" then
         return Util.Beans.Objects.To_Object (From.Mime_Type);
      elsif Name = "file_size" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.File_Size));
      elsif Name = "is_public" then
         return Util.Beans.Objects.To_Object (From.Is_Public);
      elsif Name = "width" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Width));
      elsif Name = "height" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Height));
      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;


   --  ------------------------------
   --  Set the value identified by the name
   --  ------------------------------
   overriding
   procedure Set_Value (Item  : in out Image_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "folder_id" then
         Item.Folder_Id := ADO.Identifier (Util.Beans.Objects.To_Long_Long_Integer (Value));
      elsif Name = "folder_name" then
         Item.Folder_Name := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "id" then
         Item.Id := ADO.Identifier (Util.Beans.Objects.To_Long_Long_Integer (Value));
      elsif Name = "name" then
         Item.Name := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "create_date" then
         Item.Create_Date := Util.Beans.Objects.Time.To_Time (Value);
      elsif Name = "uri" then
         Item.Uri := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "storage" then
         Item.Storage := AWA.Storages.Models.Storage_Type_Objects.To_Value (Value);
      elsif Name = "mime_type" then
         Item.Mime_Type := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "file_size" then
         Item.File_Size := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "is_public" then
         Item.Is_Public := Util.Beans.Objects.To_Boolean (Value);
      elsif Name = "width" then
         Item.Width := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "height" then
         Item.Height := Util.Beans.Objects.To_Integer (Value);
      end if;
   end Set_Value;


   --  --------------------
   --  Read in the object the data from the query result and prepare to read the next row.
   --  If there is no row, raise the ADO.NOT_FOUND exception.
   --  --------------------
   procedure Read (Into : in out Image_Bean;
                   Stmt : in out ADO.Statements.Query_Statement'Class) is
   begin
      if not Stmt.Has_Elements then
         raise ADO.Objects.NOT_FOUND;
      end if;
      Into.Folder_Id := Stmt.Get_Identifier (0);
      Into.Folder_Name := Stmt.Get_Unbounded_String (1);
      Into.Id := Stmt.Get_Identifier (2);
      Into.Name := Stmt.Get_Unbounded_String (3);
      Into.Create_Date := Stmt.Get_Time (4);
      Into.Uri := Stmt.Get_Unbounded_String (5);
      Into.Storage := AWA.Storages.Models.Storage_Type'Enum_Val (Stmt.Get_Integer (6));
      Into.Mime_Type := Stmt.Get_Unbounded_String (7);
      Into.File_Size := Stmt.Get_Integer (8);
      Into.Is_Public := Stmt.Get_Boolean (9);
      Into.Width := Stmt.Get_Integer (10);
      Into.Height := Stmt.Get_Integer (11);
      Stmt.Next;
   end Read;

   --  --------------------
   --  Run the query controlled by <b>Context</b> and load the result in <b>Object</b>.
   --  --------------------
   procedure Load (Object  : in out Image_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class) is
      Stmt : ADO.Statements.Query_Statement := Session.Create_Statement (Context);
   begin
      Stmt.Execute;
      Read (Object, Stmt);
      if Stmt.Has_Elements then
         raise ADO.Objects.NOT_FOUND;
      end if;
   end Load;


   --  ------------------------------
   --  Get the bean attribute identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Image_Info;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "id" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Id));
      elsif Name = "name" then
         return Util.Beans.Objects.To_Object (From.Name);
      elsif Name = "create_date" then
         return Util.Beans.Objects.Time.To_Object (From.Create_Date);
      elsif Name = "uri" then
         return Util.Beans.Objects.To_Object (From.Uri);
      elsif Name = "storage" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Storage));
      elsif Name = "mime_type" then
         return Util.Beans.Objects.To_Object (From.Mime_Type);
      elsif Name = "file_size" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.File_Size));
      elsif Name = "width" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Width));
      elsif Name = "height" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Height));
      elsif Name = "thumb_width" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Thumb_Width));
      elsif Name = "thumb_height" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Thumb_Height));
      elsif Name = "thumbnail_id" then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Thumbnail_Id));
      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;


   --  ------------------------------
   --  Set the value identified by the name
   --  ------------------------------
   overriding
   procedure Set_Value (Item  : in out Image_Info;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" then
         Item.Id := ADO.Identifier (Util.Beans.Objects.To_Long_Long_Integer (Value));
      elsif Name = "name" then
         Item.Name := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "create_date" then
         Item.Create_Date := Util.Beans.Objects.Time.To_Time (Value);
      elsif Name = "uri" then
         Item.Uri := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "storage" then
         Item.Storage := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "mime_type" then
         Item.Mime_Type := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "file_size" then
         Item.File_Size := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "width" then
         Item.Width := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "height" then
         Item.Height := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "thumb_width" then
         Item.Thumb_Width := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "thumb_height" then
         Item.Thumb_Height := Util.Beans.Objects.To_Integer (Value);
      elsif Name = "thumbnail_id" then
         Item.Thumbnail_Id := ADO.Identifier (Util.Beans.Objects.To_Long_Long_Integer (Value));
      end if;
   end Set_Value;


   --  --------------------
   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   --  --------------------
   procedure List (Object  : in out Image_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class) is
   begin
      List (Object.List, Session, Context);
   end List;

   --  --------------------
   --  The list of images for a given folder.
   --  --------------------
   procedure List (Object  : in out Image_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class) is
      procedure Read (Into : in out Image_Info);

      Stmt : ADO.Statements.Query_Statement
          := Session.Create_Statement (Context);
      Pos  : Positive := 1;
      procedure Read (Into : in out Image_Info) is
      begin
         Into.Id := Stmt.Get_Identifier (0);
         Into.Name := Stmt.Get_Unbounded_String (1);
         Into.Create_Date := Stmt.Get_Time (2);
         Into.Uri := Stmt.Get_Unbounded_String (3);
         Into.Storage := Stmt.Get_Integer (4);
         Into.Mime_Type := Stmt.Get_Unbounded_String (5);
         Into.File_Size := Stmt.Get_Integer (6);
         Into.Width := Stmt.Get_Integer (7);
         Into.Height := Stmt.Get_Integer (8);
         Into.Thumb_Width := Stmt.Get_Integer (9);
         Into.Thumb_Height := Stmt.Get_Integer (10);
         Into.Thumbnail_Id := Stmt.Get_Identifier (11);
      end Read;
   begin
      Stmt.Execute;
      Image_Info_Vectors.Clear (Object);
      while Stmt.Has_Elements loop
         Object.Insert_Space (Before => Pos);
         Object.Update_Element (Index => Pos, Process => Read'Access);
         Pos := Pos + 1;
         Stmt.Next;
      end loop;
   end List;



end AWA.Images.Models;
