-----------------------------------------------------------------------
--  awa-modules-lifecycles -- Lifecycle listeners
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body AWA.Modules.Lifecycles is

   --  ------------------------------
   --  Inform the the life cycle listeners registered in `List` that the item passed in `Item`
   --  has been created (calls `On_Create`).
   --  ------------------------------
   procedure Notify_Create (Service : in AWA.Modules.Module_Manager'Class;
                            Item    : in Element_Type) is
   begin
      LF.Notify_Create (Service.Module.Listeners, Item);
   end Notify_Create;

   --  ------------------------------
   --  Inform the the life cycle listeners registered in `List` that the item passed in `Item`
   --  has been created (calls `On_Create`).
   --  ------------------------------
   procedure Notify_Create (Service : in AWA.Modules.Module'Class;
                            Item    : in Element_Type) is
   begin
      LF.Notify_Create (Service.Listeners, Item);
   end Notify_Create;

   --  ------------------------------
   --  Inform the the life cycle listeners registered in `List` that the item passed in `Item`
   --  has been updated (calls `On_Update`).
   --  ------------------------------
   procedure Notify_Update (Service : in AWA.Modules.Module_Manager'Class;
                            Item    : in Element_Type) is
   begin
      LF.Notify_Update (Service.Module.Listeners, Item);
   end Notify_Update;

   --  ------------------------------
   --  Inform the the life cycle listeners registered in `List` that the item passed in `Item`
   --  has been updated (calls `On_Update`).
   --  ------------------------------
   procedure Notify_Update (Service : in AWA.Modules.Module'Class;
                            Item    : in Element_Type) is
   begin
      LF.Notify_Update (Service.Listeners, Item);
   end Notify_Update;

   --  ------------------------------
   --  Inform the the life cycle listeners registered in `List` that the item passed in `Item`
   --  has been deleted (calls `On_Delete`).
   --  ------------------------------
   procedure Notify_Delete (Service : in AWA.Modules.Module_Manager'Class;
                            Item    : in Element_Type) is
   begin
      LF.Notify_Delete (Service.Module.Listeners, Item);
   end Notify_Delete;

   --  ------------------------------
   --  Inform the the life cycle listeners registered in `List` that the item passed in `Item`
   --  has been deleted (calls `On_Delete`).
   --  ------------------------------
   procedure Notify_Delete (Service : in AWA.Modules.Module'Class;
                            Item    : in Element_Type) is
   begin
      LF.Notify_Delete (Service.Listeners, Item);
   end Notify_Delete;

end AWA.Modules.Lifecycles;
