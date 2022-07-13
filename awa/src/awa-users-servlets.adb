-----------------------------------------------------------------------
--  awa-users-servlets -- OpenID verification servlet for user authentication
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2018, 2022 Stephane Carrez
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
with Util.Beans.Objects;
with Util.Beans.Objects.Records;
with Util.Log.Loggers;

with ASF.Cookies;
with ASF.Servlets;

with AWA.Users.Services;
with AWA.Users.Modules;
with AWA.Users.Filters;
with AWA.Users.Principals;
package body AWA.Users.Servlets is

   --  Name of the session attribute which holds the URI to redirect after authentication.
   REDIRECT_ATTRIBUTE : constant String := "awa-redirect";

   --  Name of the request attribute that contains the URI to redirect after authentication.
   REDIRECT_PARAM     : constant String := "redirect";

   --  Name of the session attribute which holds information about the active authentication.
   OPENID_ASSOC_ATTRIBUTE : constant String := "openid-assoc";

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Users.Servlets");

   --  Make a package to store the Association in the session.
   package Association_Bean is new Util.Beans.Objects.Records (Security.Auth.Association);

   subtype Association_Access is Association_Bean.Element_Type_Access;

   function Get_Provider_URL (Server   : in Request_Auth_Servlet;
                              Request  : in ASF.Requests.Request'Class) return String;

   function Get_Provider_URL (Server   : in Request_Auth_Servlet;
                              Request  : in ASF.Requests.Request'Class) return String is
      pragma Unreferenced (Server);

      URI  : constant String := Request.Get_Path_Info;
   begin
      if URI'Length = 0 then
         return "";
      end if;
      Log.Info ("OpenID authentication with {0}", URI);
      return URI (URI'First + 1 .. URI'Last);
   end Get_Provider_URL;

   --  ------------------------------
   --  Proceed to the OpenID authentication with an OpenID provider.
   --  Find the OpenID provider URL and starts the discovery, association phases
   --  during which a private key is obtained from the OpenID provider.
   --  After OpenID discovery and association, the user will be redirected to
   --  the OpenID provider.
   --  ------------------------------
   overriding
   procedure Do_Get (Server   : in Request_Auth_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class) is

      Ctx      : constant ASF.Servlets.Servlet_Registry_Access := Server.Get_Servlet_Context;
      Name     : constant String := Get_Provider_URL (Server, Request);
      URL      : constant String := Ctx.Get_Init_Parameter ("auth.url." & Name);
   begin
      Log.Info ("GET: request OpenId authentication to {0} - {1}", Name, URL);

      if Name'Length = 0 or else URL'Length = 0 then
         Response.Set_Status (ASF.Responses.SC_NOT_FOUND);
         return;
      end if;

      declare
         Mgr   : Security.Auth.Manager;
         OP    : Security.Auth.End_Point;
         Bean  : constant Util.Beans.Objects.Object := Association_Bean.Create;
         Assoc : constant Association_Access := Association_Bean.To_Element_Access (Bean);
         Redirect : constant String := Request.Get_Parameter (REDIRECT_PARAM);
      begin
         Server.Initialize (Name, Mgr);

         --  Yadis discovery (get the XRDS file).  This step does nothing for OAuth.
         Mgr.Discover (URL, OP);

         --  Associate to the OpenID provider and get an end-point with a key.
         Mgr.Associate (OP, Assoc.all);

         --  Save the association in the HTTP session and
         --  redirect the user to the OpenID provider.
         declare
            Auth_URL : constant String := Mgr.Get_Authentication_URL (OP, Assoc.all);
            Session  : ASF.Sessions.Session := Request.Get_Session (Create => True);
         begin
            Log.Info ("Redirect to auth URL: {0}", Auth_URL);

            Response.Send_Redirect (Location => Auth_URL);
            Session.Set_Attribute (Name  => OPENID_ASSOC_ATTRIBUTE,
                                   Value => Bean);
            if Redirect'Length > 0 then
               Session.Set_Attribute (Name  => REDIRECT_ATTRIBUTE,
                                      Value => Util.Beans.Objects.To_Object (Redirect));
            end if;
         end;
      end;
   end Do_Get;

   --  ------------------------------
   --  Create a principal object that correspond to the authenticated user identified
   --  by the <b>Auth</b> information.  The principal will be attached to the session
   --  and will be destroyed when the session is closed.
   --  ------------------------------
   overriding
   procedure Create_Principal (Server : in Verify_Auth_Servlet;
                               Auth   : in Security.Auth.Authentication;
                               Result : out ASF.Principals.Principal_Access) is
      pragma Unreferenced (Server);
      use AWA.Users.Services;

      Manager   : constant User_Service_Access := AWA.Users.Modules.Get_User_Manager;
      Principal : AWA.Users.Principals.Principal_Access;
   begin
      Manager.Authenticate (Auth    => Auth,
                            IpAddr  => "",
                            Principal => Principal);
      Result := Principal.all'Access;
   end Create_Principal;

   --  ------------------------------
   --  Get the redirection URL that must be used after the authentication succeeded.
   --  ------------------------------
   function Get_Redirect_URL (Server  : in Verify_Auth_Servlet;
                              Session : in ASF.Sessions.Session'Class;
                              Request : in ASF.Requests.Request'Class) return String is
      Redir : constant Util.Beans.Objects.Object := Session.Get_Attribute (REDIRECT_ATTRIBUTE);
      Ctx   : constant ASF.Servlets.Servlet_Registry_Access := Server.Get_Servlet_Context;
   begin
      if not Util.Beans.Objects.Is_Null (Redir) then
         return Util.Beans.Objects.To_String (Redir);
      end if;
      declare
         Cookies : constant ASF.Cookies.Cookie_Array := Request.Get_Cookies;
      begin
         for I in Cookies'Range loop
            if ASF.Cookies.Get_Name (Cookies (I)) = AWA.Users.Filters.REDIRECT_COOKIE then
               return ASF.Cookies.Get_Value (Cookies (I));
            end if;
         end loop;
      end;
      return Ctx.Get_Init_Parameter ("openid.success_url");
   end Get_Redirect_URL;

   --  ------------------------------
   --  Verify the authentication result that was returned by the OpenID provider.
   --  If the authentication succeeded and the signature was correct, sets a
   --  user principals on the session.
   --  ------------------------------
   overriding
   procedure Do_Get (Server   : in Verify_Auth_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class) is
      use type Security.Auth.Auth_Result;

      type Auth_Params is new Security.Auth.Parameters with null record;

      overriding
      function Get_Parameter (Params : in Auth_Params;
                              Name   : in String) return String;

      overriding
      function Get_Parameter (Params : in Auth_Params;
                              Name   : in String) return String is
         pragma Unreferenced (Params);
      begin
         return Request.Get_Parameter (Name);
      end Get_Parameter;

      Session    : ASF.Sessions.Session := Request.Get_Session (Create => False);
      Bean       : Util.Beans.Objects.Object;
      Mgr        : Security.Auth.Manager;
      Assoc      : Association_Access;
      Credential : Security.Auth.Authentication;
      Params     : Auth_Params;
   begin
      Log.Info ("GET: verify openid authentication");

      if not Session.Is_Valid then
         Log.Warn ("Session has expired during OpenID authentication process");
         Response.Set_Status (ASF.Responses.SC_FORBIDDEN);
         return;
      end if;

      Bean := Session.Get_Attribute (OPENID_ASSOC_ATTRIBUTE);

      --  Cleanup the session and drop the association end point.
      Session.Remove_Attribute (OPENID_ASSOC_ATTRIBUTE);
      if Util.Beans.Objects.Is_Null (Bean) then
         Log.Warn ("Verify openid request without active session");
         Response.Set_Status (ASF.Responses.SC_FORBIDDEN);
         return;
      end if;

      Assoc := Association_Bean.To_Element_Access (Bean);
      Server.Initialize (Security.Auth.Get_Provider (Assoc.all), Mgr);

      --  Verify that what we receive through the callback matches the association key.
      Mgr.Verify (Assoc.all, Params, Credential);
      if Security.Auth.Get_Status (Credential) /= Security.Auth.AUTHENTICATED then
         Log.Info ("Authentication has failed");
         Response.Set_Status (ASF.Responses.SC_FORBIDDEN);
         return;
      end if;

      Log.Info ("Authentication succeeded for {0}", Security.Auth.Get_Email (Credential));

      --  Get a user principal and set it on the session.
      declare
         User     : ASF.Principals.Principal_Access;
         Redirect : constant String
           := Verify_Auth_Servlet'Class (Server).Get_Redirect_URL (Session, Request);
      begin
         Verify_Auth_Servlet'Class (Server).Create_Principal (Credential, User);
         Session.Set_Principal (User);
         Session.Remove_Attribute (REDIRECT_ATTRIBUTE);

         Log.Info ("Redirect user to URL: {0}", Redirect);
         Response.Send_Redirect (Redirect);
      end;
   end Do_Get;

end AWA.Users.Servlets;
