-----------------------------------------------------------------------
--  awa-services -- Services
--  Copyright (C) 2011 Stephane Carrez
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

with Ada.Task_Attributes;
with ADO.Databases;

package body AWA.Services.Contexts is

   use type ADO.Databases.Connection_Status;
   use type AWA.Users.Principals.Principal_Access;

   package Task_Context is new Ada.Task_Attributes
     (Service_Context_Access, null);

   --  ------------------------------
   --  Get the application associated with the current service operation.
   --  ------------------------------
   function Get_Application (Ctx : in Service_Context)
                             return AWA.Applications.Application_Access is
   begin
      return Ctx.Application;
   end Get_Application;

   --  ------------------------------
   --  Get the current database connection for reading.
   --  ------------------------------
   function Get_Session (Ctx : in Service_Context_Access) return ADO.Sessions.Session is
   begin
      if Ctx.Slave.Get_Status /= ADO.Databases.OPEN then
         Ctx.Slave  := Ctx.Application.Get_Session;
      end if;
      return Ctx.Slave;
   end Get_Session;

   --  ------------------------------
   --  Get the current database connection for reading and writing.
   --  ------------------------------
   function Get_Master_Session (Ctx : in Service_Context_Access)
                                return ADO.Sessions.Master_Session is
   begin
      if Ctx.Master.Get_Status /= ADO.Databases.OPEN then
         Ctx.Master := Ctx.Application.Get_Master_Session;
--           Ctx.Slave  := Ctx.Master;
      end if;
      return Ctx.Master;
   end Get_Master_Session;

   --  ------------------------------
   --  Get the current user invoking the service operation.
   --  Returns a null user if there is none.
   --  ------------------------------
   function Get_User (Ctx : in Service_Context) return AWA.Users.Models.User_Ref is
   begin
      if Ctx.Principal = null then
         return AWA.Users.Models.Null_User;
      else
         return Ctx.Principal.Get_User;
      end if;
   end Get_User;

   --  ------------------------------
   --  Get the current user identifier invoking the service operation.
   --  Returns NO_IDENTIFIER if there is none.
   --  ------------------------------
   function Get_User_Identifier (Ctx : in Service_Context) return ADO.Identifier is
   begin
      if Ctx.Principal = null then
         return ADO.NO_IDENTIFIER;
      else
         return Ctx.Principal.Get_User_Identifier;
      end if;
   end Get_User_Identifier;

   --  ------------------------------
   --     function Get_Service (Ctx : in Service_Context; Service : in Service_Id)
   -- return Abstract_Service;

   --  ------------------------------
   --  Starts a transaction.
   --  ------------------------------
   procedure Start (Ctx : in out Service_Context) is
   begin
      if Ctx.Transaction = 0 and then Ctx.Master.Get_Status /= ADO.Databases.OPEN then
         Ctx.Master.Begin_Transaction;
         Ctx.Active_Transaction := True;
      end if;
      Ctx.Transaction := Ctx.Transaction + 1;
   end Start;

   --  ------------------------------
   --  Commits the current transaction.  The database transaction is really committed by the
   --  last <b>Commit</b> called.
   --  ------------------------------
   procedure Commit (Ctx : in out Service_Context) is
   begin
      Ctx.Transaction := Ctx.Transaction - 1;
      if Ctx.Transaction = 0 and then Ctx.Active_Transaction then
         Ctx.Master.Commit;
         Ctx.Active_Transaction := False;
      end if;
   end Commit;

   --  ------------------------------
   --  Rollback the current transaction.  The database transaction is rollback at the first
   --  call to <b>Rollback</b>.
   --  ------------------------------
   procedure Rollback (Ctx : in out Service_Context) is
   begin
      null;
   end Rollback;

   --  ------------------------------
   --  Set the current application and user context.
   --  ------------------------------
   procedure Set_Context (Ctx         : in out Service_Context;
                          Application : in AWA.Applications.Application_Access;
                          Principal   : in AWA.Users.Principals.Principal_Access) is
   begin
      Ctx.Application := Application;
      Ctx.Principal   := Principal;
   end Set_Context;

   --  ------------------------------
   --  Initializes the service context.
   --  ------------------------------
   overriding
   procedure Initialize (Ctx : in out Service_Context) is
   begin
      Ctx.Previous := Task_Context.Value;
      Task_Context.Set_Value (Ctx'Unchecked_Access);
   end Initialize;

   --  ------------------------------
   --  Finalize the service context, rollback non-committed transaction, releases any object.
   --  ------------------------------
   overriding
   procedure Finalize (Ctx : in out Service_Context) is
   begin
      --  When the service context is released, we must not have any active transaction.
      --  This means we are leaving the service in an abnormal way such as when an
      --  exception is raised.  If this is the case, rollback the transaction.
      if Ctx.Active_Transaction then
         Ctx.Master.Rollback;
      end if;
      Task_Context.Set_Value (Ctx.Previous);
   end Finalize;

   --  ------------------------------
   --  Get the current service context.
   --  Returns null if the current thread is not associated with any service context.
   --  ------------------------------
   function Current return Service_Context_Access is
   begin
      return Task_Context.Value;
   end Current;

end AWA.Services.Contexts;
