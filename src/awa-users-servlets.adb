-----------------------------------------------------------------------
--  awa-users-servlets -- OpenID verification servlet for user authentication
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2018, 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Objects;
with Util.Beans.Objects.Records;
with Util.Log.Loggers;

with AWA.Users.Services;
with AWA.Users.Modules;
with AWA.Users.Filters;
package body AWA.Users.Servlets is

   package UBO renames Util.Beans.Objects;

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
                              Request  : in Servlet.Requests.Request'Class) return String;

   --  ------------------------------
   --  Set the user principal on the session associated with the ASF request.
   --  ------------------------------
   procedure Set_Session_Principal (Request   : in out Servlet.Requests.Request'Class;
                                    Principal : in Principals.Principal_Access) is
      Session   : Servlet.Sessions.Session := Request.Get_Session (Create => True);
   begin
      Session.Set_Principal (Principal.all'Access);
   end Set_Session_Principal;

   function Get_Provider_URL (Server   : in Request_Auth_Servlet;
                              Request  : in Servlet.Requests.Request'Class) return String is
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
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class) is

      Ctx      : constant Servlet.Core.Servlet_Registry_Access := Server.Get_Servlet_Context;
      Name     : constant String := Get_Provider_URL (Server, Request);
      URL      : constant String := Ctx.Get_Init_Parameter ("auth.url." & Name);
      Session  : Servlet.Sessions.Session := Request.Get_Session (Create => True);
   begin
      Log.Info ("GET: request OpenId authentication to {0} - {1}", Name, URL);

      if Name'Length = 0 or else URL'Length = 0 then
         Session.Set_Attribute (AUTH_ERROR_ATTRIBUTE, UBO.To_Object (MESSAGE_BAD_CONFIGURATION));
         Response.Set_Status (Servlet.Responses.SC_NOT_FOUND);
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
         begin
            Log.Info ("Redirect to auth URL: {0}", Auth_URL);

            Response.Send_Redirect (Location => Auth_URL);
            Session.Set_Attribute (Name  => OPENID_ASSOC_ATTRIBUTE,
                                   Value => Bean);
            if Redirect'Length > 0 then
               Session.Set_Attribute (Name  => REDIRECT_ATTRIBUTE,
                                      Value => Util.Beans.Objects.To_Object (Redirect));
               AWA.Users.Filters.Clear_Redirect_Cookie (Request, Response);
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
                               Result : out Security.Principal_Access) is
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
                              Session : in Servlet.Sessions.Session'Class;
                              Request : in Servlet.Requests.Request'Class) return String is
      Redir : constant Util.Beans.Objects.Object := Session.Get_Attribute (REDIRECT_ATTRIBUTE);
      Ctx   : constant Servlet.Core.Servlet_Registry_Access := Server.Get_Servlet_Context;
   begin
      if not Util.Beans.Objects.Is_Null (Redir) then
         return Util.Beans.Objects.To_String (Redir);
      end if;
      declare
         URL : constant String := AWA.Users.Filters.Get_Redirect_Cookie (Request);
      begin
         if URL'Length > 0 then
            return URL;
         else
            return Ctx.Get_Init_Parameter ("openid.success_url");
         end if;
      end;
   end Get_Redirect_URL;

   --  ------------------------------
   --  Get the redirection URL that must be used after the authentication failed.
   --  ------------------------------
   function Get_Error_URL (Server  : in Verify_Auth_Servlet;
                           Session : in Servlet.Sessions.Session'Class;
                           Request : in Servlet.Requests.Request'Class) return String is
      pragma Unreferenced (Session, Request);

      Ctx   : constant Servlet.Core.Servlet_Registry_Access := Server.Get_Servlet_Context;
   begin
      return Ctx.Get_Init_Parameter ("openid.error_url");
   end Get_Error_URL;

   --  ------------------------------
   --  Verify the authentication result that was returned by the OpenID provider.
   --  If the authentication succeeded and the signature was correct, sets a
   --  user principals on the session.
   --  ------------------------------
   overriding
   procedure Do_Get (Server   : in Verify_Auth_Servlet;
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class) is
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

      Session    : Servlet.Sessions.Session := Request.Get_Session (Create => False);
      Bean       : Util.Beans.Objects.Object;
      Mgr        : Security.Auth.Manager;
      Assoc      : Association_Access;
      Credential : Security.Auth.Authentication;
      Params     : Auth_Params;
      Error_URI  : constant String
        := Verify_Auth_Servlet'Class (Server).Get_Error_URL (Session, Request);
   begin
      Log.Info ("GET: verify openid authentication");

      if not Session.Is_Valid then
         Log.Warn ("Session has expired during OpenID authentication process, "
                   & "redirect to {0}", Error_URI);
         Response.Send_Redirect (Error_URI);
         return;
      end if;

      Bean := Session.Get_Attribute (OPENID_ASSOC_ATTRIBUTE);

      --  Cleanup the session and drop the association end point.
      Session.Remove_Attribute (OPENID_ASSOC_ATTRIBUTE);
      if Util.Beans.Objects.Is_Null (Bean) then
         Log.Warn ("Verify openid request without active session, "
                   & "redirect to {0}", Error_URI);
         Response.Send_Redirect (Error_URI);
         return;
      end if;

      Assoc := Association_Bean.To_Element_Access (Bean);
      Server.Initialize (Security.Auth.Get_Provider (Assoc.all), Mgr);

      --  Verify that what we receive through the callback matches the association key.
      Mgr.Verify (Assoc.all, Params, Credential);
      if Security.Auth.Get_Status (Credential) /= Security.Auth.AUTHENTICATED then
         Log.Info ("Authentication has failed, redirect to {0}", Error_URI);
         Session.Set_Attribute (AUTH_ERROR_ATTRIBUTE, UBO.To_Object (MESSAGE_AUTH_FAILED));
         Response.Send_Redirect (Error_URI);
         return;
      end if;

      Log.Info ("Authentication succeeded for {0}", Security.Auth.Get_Email (Credential));

      --  Get a user principal and set it on the session.
      declare
         User     : Security.Principal_Access;
         Redirect : constant String
           := Verify_Auth_Servlet'Class (Server).Get_Redirect_URL (Session, Request);
      begin
         Verify_Auth_Servlet'Class (Server).Create_Principal (Credential, User);
         Session.Set_Principal (User);
         Session.Remove_Attribute (REDIRECT_ATTRIBUTE);

         Log.Info ("Redirect user to URL: {0}", Redirect);
         Response.Send_Redirect (Redirect);
         AWA.Users.Filters.Clear_Redirect_Cookie (Request, Response);

      exception
         when AWA.Users.Services.Registration_Disabled =>
            Log.Info ("User registration is disabled, redirect to {0}", Error_URI);
            Session.Set_Attribute (AUTH_ERROR_ATTRIBUTE,
                                   UBO.To_Object (MESSAGE_REGISTRATION_DISABLED));
            Response.Send_Redirect (Error_URI);

      end;
   end Do_Get;

   --  ------------------------------
   --  Get the redirection URL that must be used after the authentication succeeded.
   --  ------------------------------
   function Get_Redirect_URL (Server  : in Verify_Key_Servlet;
                              Request : in Servlet.Requests.Request'Class) return String is
      Ctx : constant Servlet.Core.Servlet_Registry_Access := Server.Get_Servlet_Context;
      URL : constant String := AWA.Users.Filters.Get_Redirect_Cookie (Request);
   begin
      if URL'Length > 0 then
         return URL;
      else
         return Ctx.Get_Init_Parameter ("openid.success_url");
      end if;
   end Get_Redirect_URL;

   --  ------------------------------
   --  Initialize the filter and configure the redirection URIs.
   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Verify_Key_Servlet;
                         Context : in Servlet.Core.Servlet_Registry'Class) is
      use Servlet.Core;
      use Ada.Strings.Unbounded;

      URI : constant String := Context.Get_Init_Parameter (VERIFY_FILTER_REDIRECT_PARAM);
   begin
      Server.Invalid_Key_URI := To_Unbounded_String (URI);
      if URI'Length = 0 then
         Log.Error ("Missing configuration for {0}.{1}",
                    Server.Get_Name,
                    VERIFY_FILTER_REDIRECT_PARAM);
      end if;
      Server.Change_Password_URI
        := Context.Get_Init_Parameter (VERIFY_FILTER_CHANGE_PASSWORD_PARAM);
   end Initialize;

   --  ------------------------------
   --  Filter a request which contains an access key and verify that the
   --  key is valid and identifies a user.  Once the user is known, create
   --  a session and setup the user principal.
   --
   --  If the access key is missing or invalid, redirect to the
   --  <b>Invalid_Key_URI</b> associated with the filter.
   --  ------------------------------
   overriding
   procedure Do_Get (Server   : in Verify_Key_Servlet;
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class) is
      use type AWA.Users.Principals.Principal_Access;
      use Ada.Strings.Unbounded;

      Key       : constant String := Request.Get_Path_Parameter (1);
      Manager   : constant Users.Services.User_Service_Access := Users.Modules.Get_User_Manager;
      Principal : AWA.Users.Principals.Principal_Access;
      Session   : Servlet.Sessions.Session := Request.Get_Session (Create => True);
   begin
      Log.Info ("Verify access key '{0}'", Key);

      Manager.Verify_User (Key       => Key,
                           IpAddr    => "",
                           Principal => Principal);

      if Principal = null then
         declare
            URI : constant String := To_String (Server.Change_Password_URI) & Key;
         begin
            Log.Info ("Access key verified but no password, redirecting to {0}", URI);
            Response.Send_Redirect (Location => URI);
            return;
         end;
      end if;

      Session.Set_Principal (Principal.all'Access);

      --  Request is authorized, redirect to the final page.
      Response.Send_Redirect (Location => Server.Get_Redirect_URL (Request));

   exception
      when AWA.Users.Services.Not_Found =>
         declare
            URI : constant String := To_String (Server.Invalid_Key_URI);
         begin
            Log.Info ("Invalid access key '{0}', redirecting to {1}", Key, URI);
            Session.Set_Attribute (AUTH_ERROR_ATTRIBUTE,
                                   UBO.To_Object (MESSAGE_INVALID_KEY));
            Response.Send_Redirect (Location => URI);
         end;
   end Do_Get;

end AWA.Users.Servlets;
