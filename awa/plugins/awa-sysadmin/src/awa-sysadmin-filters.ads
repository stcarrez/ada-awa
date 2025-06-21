-----------------------------------------------------------------------
--  awa-users-filters -- Specific filters for authentication and key verification
--  Copyright (C) 2011, 2012, 2015, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Servlet.Requests;
with Servlet.Responses;
with Servlet.Filters;
with Servlet.Core;

with AWA.Applications;
package AWA.Sysadmin.Filters is

   ADMIN_AUTH_BEAN : constant String := "sysadminAuth";

   --  ------------------------------
   --  Authentication verification filter
   --  ------------------------------
   --  The <b>Auth_Filter</b> verifies that the user has the permission to access
   --  a given page.  If the user is not logged, it tries to login automatically
   --  by using some persistent cookie.  When this fails, it redirects the
   --  user to a login page (configured by AUTH_FILTER_REDIRECT_PARAM property).
   type Auth_Filter is new Servlet.Filters.Filter with private;

   --  Initialize the filter and configure the redirection URIs.
   overriding
   procedure Initialize (Filter  : in out Auth_Filter;
                         Config  : in Servlet.Core.Filter_Config);

   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   procedure Do_Login (Filter   : in Auth_Filter;
                       Request  : in out Servlet.Requests.Request'Class;
                       Response : in out Servlet.Responses.Response'Class);

   --  Filter a request which contains an access key and verify that the
   --  key is valid and identifies a user.  Once the user is known, create
   --  a session and setup the user principal.
   --
   --  If the access key is missing or invalid, redirect to the
   --  <b>Invalid_Key_URI</b> associated with the filter.
   overriding
   procedure Do_Filter (Filter   : in Auth_Filter;
                        Request  : in out Servlet.Requests.Request'Class;
                        Response : in out Servlet.Responses.Response'Class;
                        Chain    : in out Servlet.Core.Filter_Chain);

private

   use Ada.Strings.Unbounded;

   type Auth_Filter is new Servlet.Filters.Filter with record
      Login_URI   : Unbounded_String;
      Application : AWA.Applications.Application_Access;
   end record;

end AWA.Sysadmin.Filters;
