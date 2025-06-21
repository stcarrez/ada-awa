-----------------------------------------------------------------------
--  awa-modules-lifecycles -- Lifecycle listeners
--  Copyright (C) 2012, 2013, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Listeners.Lifecycles;

generic
   type Element_Type (<>) is limited private;
package AWA.Modules.Lifecycles is

   package LF is new Util.Listeners.Lifecycles (Element_Type);

   subtype Listener is LF.Listener;

   --  Inform the the life cycle listeners registered in `List` that the item passed in `Item`
   --  has been created (calls `On_Create`).
   procedure Notify_Create (Service : in AWA.Modules.Module_Manager'Class;
                            Item    : in Element_Type);
   procedure Notify_Create (Service : in AWA.Modules.Module'Class;
                            Item    : in Element_Type);
   pragma Inline (Notify_Create);

   --  Inform the the life cycle listeners registered in `List` that the item passed in `Item`
   --  has been updated (calls `On_Update`).
   procedure Notify_Update (Service : in AWA.Modules.Module_Manager'Class;
                            Item    : in Element_Type);
   procedure Notify_Update (Service : in AWA.Modules.Module'Class;
                            Item    : in Element_Type);
   pragma Inline (Notify_Update);

   --  Inform the the life cycle listeners registered in `List` that the item passed in `Item`
   --  has been deleted (calls `On_Delete`).
   procedure Notify_Delete (Service : in AWA.Modules.Module_Manager'Class;
                            Item    : in Element_Type);
   procedure Notify_Delete (Service : in AWA.Modules.Module'Class;
                            Item    : in Element_Type);
   pragma Inline (Notify_Delete);

end AWA.Modules.Lifecycles;
