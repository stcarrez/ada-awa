-----------------------------------------------------------------------
--  awa-storages-servlets -- Serve files saved in the storage service
--  Copyright (C) 2012, 2016, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Calendar;

with Servlet.Core;
with ASF.Requests;
with ASF.Responses;

--  == Storage Servlet ==
--  The <tt>Storage_Servlet</tt> type is the servlet that allows to retrieve the file
--  content that was uploaded.
package AWA.Storages.Servlets is

   type Get_Type is (DEFAULT, AS_CONTENT_DISPOSITION, INVALID);

   --  The <b>Storage_Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   type Storage_Servlet is new Servlet.Core.Servlet with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Storage_Servlet;
                         Context : in Servlet.Core.Servlet_Registry'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a GET request.
   --
   --  Overriding this method to support a GET request also automatically supports
   --  an HTTP HEAD request. A HEAD request is a GET request that returns no body
   --  in the response, only the request header fields.
   --
   --  When overriding this method, read the request data, write the response headers,
   --  get the response's writer or output stream object, and finally, write the
   --  response data. It's best to include content type and encoding.
   --  When using a PrintWriter object to return the response, set the content type
   --  before accessing the PrintWriter object.
   --
   --  The servlet container must write the headers before committing the response,
   --  because in HTTP the headers must be sent before the response body.
   --
   --  Where possible, set the Content-Length header (with the
   --  Response.Set_Content_Length method), to allow the servlet container
   --  to use a persistent connection to return its response to the client,
   --  improving performance. The content length is automatically set if the entire
   --  response fits inside the response buffer.
   --
   --  When using HTTP 1.1 chunked encoding (which means that the response has a
   --  Transfer-Encoding header), do not set the Content-Length header.
   --
   --  The GET method should be safe, that is, without any side effects for which
   --  users are held responsible. For example, most form queries have no side effects.
   --  If a client request is intended to change stored data, the request should use
   --  some other HTTP method.
   --
   --  The GET method should also be idempotent, meaning that it can be safely repeated.
   --  Sometimes making a method safe also makes it idempotent. For example, repeating
   --  queries is both safe and idempotent, but buying a product online or modifying
   --  data is neither safe nor idempotent.
   --
   --  If the request is incorrectly formatted, Do_Get  returns an HTTP "Bad Request"
   overriding
   procedure Do_Get (Server   : in Storage_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class);

   --  Load the data content that correspond to the GET request and get the name as well
   --  as mime-type and date.
   procedure Load (Server   : in Storage_Servlet;
                   Request  : in out ASF.Requests.Request'Class;
                   Name     : out Ada.Strings.Unbounded.Unbounded_String;
                   Mime     : out Ada.Strings.Unbounded.Unbounded_String;
                   Date     : out Ada.Calendar.Time;
                   Data     : out ADO.Blob_Ref);

   --  Get the expected return mode (content disposition for download or inline).
   function Get_Format (Server   : in Storage_Servlet;
                        Request  : in ASF.Requests.Request'Class) return Get_Type;

private

   type Storage_Servlet is new Servlet.Core.Servlet with null record;

end AWA.Storages.Servlets;
