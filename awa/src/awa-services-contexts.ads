-----------------------------------------------------------------------
--  awa-services -- Services
--  Copyright (C) 2011, 2012, 2013, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Applications;
with AWA.Users.Principals;
with AWA.Users.Models;

with ADO;
with ADO.Sessions;
with Util.Beans.Objects;

with Ada.Finalization;

--  The service context provides additional information to a service operation.
--  This context is composed of:
--  <ul>
--    <li>the optional user context which allows to identify the current user invoking the service,
--    <li>the database connections that the service can use,
--    <li>the other services provided by the application.
--  </ul>
package AWA.Services.Contexts is

   type Service_Context is new Ada.Finalization.Limited_Controlled with private;
   type Service_Context_Access is access all Service_Context'Class;

   --  Get the application associated with the current service operation.
   function Get_Application (Ctx : in Service_Context) return AWA.Applications.Application_Access;

   --  Get the current database connection for reading.
   function Get_Session (Ctx : in Service_Context_Access) return ADO.Sessions.Session;

   --  Get the current database connection for reading and writing.
   function Get_Master_Session (Ctx : in Service_Context_Access)
                                return ADO.Sessions.Master_Session;

   --  Get the current user invoking the service operation.
   --  Returns a null user if there is none.
   function Get_User (Ctx : in Service_Context) return AWA.Users.Models.User_Ref;

   --  Get the current user identifier invoking the service operation.
   --  Returns NO_IDENTIFIER if there is none.
   function Get_User_Identifier (Ctx : in Service_Context) return ADO.Identifier;

   --  Get the current user session from the user invoking the service operation.
   --  Returns a null session if there is none.
   function Get_User_Session (Ctx : in Service_Context) return AWA.Users.Models.Session_Ref;

   --  Starts a transaction.
   procedure Start (Ctx : in out Service_Context);

   --  Commits the current transaction.  The database transaction is really committed by the
   --  last <b>Commit</b> called.
   procedure Commit (Ctx : in out Service_Context);

   --  Rollback the current transaction.  The database transaction is rollback at the first
   --  call to <b>Rollback</b>.
   procedure Rollback (Ctx : in out Service_Context);

   --  Get the attribute registered under the given name in the HTTP session.
   function Get_Session_Attribute (Ctx  : in Service_Context;
                                   Name : in String) return Util.Beans.Objects.Object;

   --  Set the attribute registered under the given name in the HTTP session.
   procedure Set_Session_Attribute (Ctx   : in out Service_Context;
                                    Name  : in String;
                                    Value : in Util.Beans.Objects.Object);

   --  Initializes the service context.
   overriding
   procedure Initialize (Ctx : in out Service_Context);

   --  Finalize the service context, rollback non-committed transaction, releases any object.
   overriding
   procedure Finalize (Ctx : in out Service_Context);

   --  Set the current application and user context.
   procedure Set_Context (Ctx         : in out Service_Context;
                          Application : in AWA.Applications.Application_Access;
                          Principal   : in AWA.Users.Principals.Principal_Access);

   --  Get the current service context.
   --  Returns null if the current thread is not associated with any service context.
   function Current return Service_Context_Access;

   --  Run the process procedure on behalf of the specific user and session.
   --  This operation changes temporarily the identity of the current user principal and
   --  executes the <tt>Process</tt> procedure.
   generic
      with procedure Process;
   procedure Run_As (User    : in AWA.Users.Models.User_Ref;
                     Session : in AWA.Users.Models.Session_Ref);

private

   type Service_Context is new Ada.Finalization.Limited_Controlled with record
      Previous    : Service_Context_Access := null;
      Application : AWA.Applications.Application_Access := null;
      Principal   : AWA.Users.Principals.Principal_Access := null;
      Master      : ADO.Sessions.Master_Session;
      Slave       : ADO.Sessions.Session;
      Transaction : Integer := 0;
      Active_Transaction : Boolean := False;
   end record;

end AWA.Services.Contexts;
