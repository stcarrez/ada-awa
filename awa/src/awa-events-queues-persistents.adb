-----------------------------------------------------------------------
--  awa-events-queues-persistents -- AWA Event Queues
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
with AWA.Services.Contexts;
with ADO;
with ADO.Queries;
with ADO.Sessions;
package body AWA.Events.Queues.Persistents is

   --  ------------------------------
   --  Get the queue name.
   --  ------------------------------
   function Get_Name (From : in Persistent_Queue) return String is
   begin
      return From.Name;
   end Get_Name;

   procedure Enqueue (Into  : in out Persistent_Queue;
                      Event : in AWA.Events.Module_Event'Class) is
      Ctx   : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Msg   : AWA.Events.Models.Message_Ref;
   begin
      Ctx.Start;
      Msg.Set_Queue (Into.Queue);
      Msg.Set_User (Ctx.Get_User);
      Msg.Set_Session (Ctx.Get_User_Session);
      Msg.Set_Create_Date (Event.Get_Time);
      Msg.Set_Status (AWA.Events.Models.QUEUED);
--        Msg.Set_Message_Type
      Msg.Save (DB);
      Ctx.Commit;
   end Enqueue;

   procedure Dequeue (From : in out Persistent_Queue;
                      Process : access procedure (Event : in Module_Event'Class)) is
   begin
      null;
   end Dequeue;

end AWA.Events.Queues.Persistents;
