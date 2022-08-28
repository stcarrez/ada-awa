-----------------------------------------------------------------------
--  awa-services -- Services
--  Copyright (C) 2011, 2012, 2013, 2014, 2016, 2017, 2022 Stephane Carrez
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
with Util.Log.Loggers;
with Security.Contexts;

package body AWA.Services.Contexts is

   use type ADO.Sessions.Connection_Status;
   use type AWA.Users.Principals.Principal_Access;

   package Task_Context is new Ada.Task_Attributes
     (Service_Context_Access, null);

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Services.Contexts");

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
      if Ctx = null then
         Log.Error ("No AWA service context: may be a 'filter-mapping'"
                      & " is missing to activate the 'service' filter in the request path");
      end if;

      --  If a master database session was created, use it.
      if Ctx.Master.Get_Status = ADO.Sessions.OPEN then
         return ADO.Sessions.Session (Ctx.Master);

      elsif Ctx.Slave.Get_Status /= ADO.Sessions.OPEN then
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
      if Ctx.Master.Get_Status /= ADO.Sessions.OPEN then
         Ctx.Master := Ctx.Application.Get_Master_Session;
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
   --  Get the current user session from the user invoking the service operation.
   --  Returns a null session if there is none.
   --  ------------------------------
   function Get_User_Session (Ctx : in Service_Context) return AWA.Users.Models.Session_Ref is
   begin
      if Ctx.Principal = null then
         return AWA.Users.Models.Null_Session;
      else
         return Ctx.Principal.Get_Session;
      end if;
   end Get_User_Session;

   --  ------------------------------
   --  Starts a transaction.
   --  ------------------------------
   procedure Start (Ctx : in out Service_Context) is
   begin
      if Ctx.Transaction = 0 and then not Ctx.Active_Transaction then
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
   --  Get the attribute registered under the given name in the HTTP session.
   --  ------------------------------
   function Get_Session_Attribute (Ctx  : in Service_Context;
                                   Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Ctx, Name);
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Session_Attribute;

   --  ------------------------------
   --  Set the attribute registered under the given name in the HTTP session.
   --  ------------------------------
   procedure Set_Session_Attribute (Ctx   : in out Service_Context;
                                    Name  : in String;
                                    Value : in Util.Beans.Objects.Object) is
   begin
      null;
   end Set_Session_Attribute;

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
      use type AWA.Applications.Application_Access;
   begin
      Ctx.Previous := Task_Context.Value;
      Task_Context.Set_Value (Ctx'Unchecked_Access);
      if Ctx.Previous /= null and then Ctx.Application = null then
         Ctx.Application := Ctx.Previous.Application;
      end if;
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
         begin
            Ctx.Master.Rollback;

         exception
            when E : others =>
               Log.Error ("Transaction rollback failed: {0}", E);
         end;
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

   --  ------------------------------
   --  Run the process procedure on behalf of the specific user and session.
   --  This operation changes temporarily the identity of the current user principal and
   --  executes the <tt>Process</tt> procedure.
   --  ------------------------------
   procedure Run_As (User    : in AWA.Users.Models.User_Ref;
                     Session : in AWA.Users.Models.Session_Ref) is
      Ctx       : Service_Context;
      Sec       : Security.Contexts.Security_Context;
      Principal : aliased AWA.Users.Principals.Principal
        := AWA.Users.Principals.Create (User, Session);
   begin
      Ctx.Principal := Principal'Unchecked_Access;
      Sec.Set_Context (Ctx.Application.Get_Security_Manager, Principal'Unchecked_Access);
      Process;
   end Run_As;

end AWA.Services.Contexts;
