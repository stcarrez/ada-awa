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
with Util.Listeners.Lifecycles;

generic
   type Element_Type (<>) is limited private;
package AWA.Modules.Lifecycles is

   package LF is new Util.Listeners.Lifecycles (Element_Type);

   subtype Listener is LF.Listener;

   --  Inform the the lifecycle listeners registered in `List` that the item passed in `Item`
   --  has been created (calls `On_Create`).
   procedure Notify_Create (Service : in AWA.Modules.Module_Manager'Class;
                            Item    : in Element_Type);
   pragma Inline (Notify_Create);

   --  Inform the the lifecycle listeners registered in `List` that the item passed in `Item`
   --  has been updated (calls `On_Update`).
   procedure Notify_Update (Service : in AWA.Modules.Module_Manager'Class;
                            Item    : in Element_Type);
   pragma Inline (Notify_Update);

   --  Inform the the lifecycle listeners registered in `List` that the item passed in `Item`
   --  has been deleted (calls `On_Delete`).
   procedure Notify_Delete (Service : in AWA.Modules.Module_Manager'Class;
                            Item    : in Element_Type);
   pragma Inline (Notify_Delete);

end AWA.Modules.Lifecycles;
