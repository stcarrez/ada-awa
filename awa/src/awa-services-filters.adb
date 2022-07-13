-----------------------------------------------------------------------
--  awa-services-filters -- Setup service context in request processing flow
--  Copyright (C) 2011, 2013, 2015, 2022 Stephane Carrez
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
with AWA.Applications;
with AWA.Users.Principals;

with ASF.Principals;
with ASF.Sessions;
with Util.Beans.Objects;

package body AWA.Services.Filters is

   --  ------------------------------
   --  The Do_Filter method of the Filter is called by the container each time
   --  a request/response pair is passed through the chain due to a client request
   --  for a resource at the end of the chain.  The Filter_Chain passed in to this
   --  method allows the Filter to pass on the request and response to the next
   --  entity in the chain.
   --
   --  Before passing the control to the next filter, initialize the service
   --  context to give access to the current application, current user and
   --  manage possible transaction rollbacks.
   --  ------------------------------
   overriding
   procedure Do_Filter (F        : in Service_Filter;
                        Request  : in out ASF.Requests.Request'Class;
                        Response : in out ASF.Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain) is

      pragma Unreferenced (F);

      use type ASF.Principals.Principal_Access;

      type Context_Type is new AWA.Services.Contexts.Service_Context with null record;

      --  Get the attribute registered under the given name in the HTTP session.
      overriding
      function Get_Session_Attribute (Ctx  : in Context_Type;
                                      Name : in String) return Util.Beans.Objects.Object;

      --  Set the attribute registered under the given name in the HTTP session.
      overriding
      procedure Set_Session_Attribute (Ctx   : in out Context_Type;
                                       Name  : in String;
                                       Value : in Util.Beans.Objects.Object);

      overriding
      function Get_Session_Attribute (Ctx  : in Context_Type;
                                      Name : in String) return Util.Beans.Objects.Object is
         pragma Unreferenced (Ctx);
      begin
         return Request.Get_Session.Get_Attribute (Name);
      end Get_Session_Attribute;

      --  Set the attribute registered under the given name in the HTTP session.
      overriding
      procedure Set_Session_Attribute (Ctx   : in out Context_Type;
                                       Name  : in String;
                                       Value : in Util.Beans.Objects.Object) is
         pragma Unreferenced (Ctx);
         S : ASF.Sessions.Session := Request.Get_Session;
      begin
         S.Set_Attribute (Name, Value);
      end Set_Session_Attribute;

      App : constant ASF.Servlets.Servlet_Registry_Access
        := ASF.Servlets.Get_Servlet_Context (Chain);
      P   : constant ASF.Principals.Principal_Access := Request.Get_User_Principal;
      Context     : aliased Context_Type;
      Principal   : AWA.Users.Principals.Principal_Access;
      Application : AWA.Applications.Application_Access;
   begin
      --  Get the user
      if P /= null and then P.all in AWA.Users.Principals.Principal'Class then
         Principal := AWA.Users.Principals.Principal'Class (P.all)'Access;
      else
         Principal := null;
      end if;

      --  Get the application
      if App.all in AWA.Applications.Application'Class then
         Application := AWA.Applications.Application'Class (App.all)'Access;
      else
         Application := null;
      end if;

      --  Setup the service context.
      Context.Set_Context (Application, Principal);

      --  Give the control to the next chain up to the servlet.
      ASF.Servlets.Do_Filter (Chain    => Chain,
                              Request  => Request,
                              Response => Response);

      --  By leaving this scope, the active database transactions are rollbacked
      --  (finalization of Service_Context)
   end Do_Filter;

end AWA.Services.Filters;
