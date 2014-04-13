-----------------------------------------------------------------------
--  awa-modules-lifecycles -- Lifecycle listeners
--  Copyright (C) 2012 Stephane Carrez
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
