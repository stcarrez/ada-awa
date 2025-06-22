-----------------------------------------------------------------------
--  awa-oauth-filters -- OAuth filter
--  Copyright (C) 2017, 2018, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Requests;
with ASF.Responses;
with ASF.Filters;
with ASF.Servlets;

with AWA.OAuth.Services;
package AWA.OAuth.Filters is

   --  ------------------------------
   --  OAuth Authentication filter
   --  ------------------------------
   --  The <b>Auth_Filter</b> verifies that the access token passed for the OAuth
   --  operation is valid and it extracts the user to configure the request principal.
   type Auth_Filter is new ASF.Filters.Filter with private;

   --  Initialize the filter.
   overriding
   procedure Initialize (Filter  : in out Auth_Filter;
                         Config  : in ASF.Servlets.Filter_Config);

   --  The Do_Filter method of the Filter is called by the container each time
   --  a request/response pair is passed through the chain due to a client request
   --  for a resource at the end of the chain.  The Filter_Chain passed in to this
   --  method allows the Filter to pass on the request and response to the next
   --  entity in the chain.
   --
   --  Before passing the control to the next filter, initialize the service
   --  context to give access to the current application, current user and
   --  manage possible transaction rollbacks.
   overriding
   procedure Do_Filter (F        : in Auth_Filter;
                        Request  : in out ASF.Requests.Request'Class;
                        Response : in out ASF.Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain);

private

   type Auth_Filter is new ASF.Filters.Filter with record
      Realm : AWA.OAuth.Services.Auth_Manager_Access;
   end record;

end AWA.OAuth.Filters;
