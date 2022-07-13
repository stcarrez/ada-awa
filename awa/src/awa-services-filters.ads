-----------------------------------------------------------------------
--  awa-services-filters -- Setup service context in request processing flow
--  Copyright (C) 2011, 2022 Stephane Carrez
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

with ASF.Filters;
with ASF.Requests;
with ASF.Responses;
with ASF.Servlets;
package AWA.Services.Filters is

   type Service_Filter is new ASF.Filters.Filter with null record;

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
   procedure Do_Filter (F        : in Service_Filter;
                        Request  : in out ASF.Requests.Request'Class;
                        Response : in out ASF.Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain);

end AWA.Services.Filters;
