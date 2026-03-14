-----------------------------------------------------------------------
--  awa-seo-servlets -- Servlets to produce sitemaps
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Servlet.Core;
with Servlet.Requests;
with Servlet.Responses;

--  == SEO Servlet ==
--  The `SEO_Servlet` type is the servlet that generates the sitemap XML
--  files by using the sitemap providers that have been registered in the
--  SEO module.  By default, the `AWA.SEO` module provides a configuration
--  to register that servlet and expose the sitemap URIs but such
--  configuration can be overriden.  The default configuration is:
--
--    <servlet-mapping>
--        <servlet-name>sitemaps</servlet-name>
--        <url-pattern>/sitemaps/*</url-pattern>
--    </servlet-mapping>
--    <filter-mapping>
--      <filter-name>service</filter-name>
--      <url-pattern>/sitemaps/*</url-pattern>
--    </filter-mapping>
package AWA.SEO.Servlets is

   --  The `SEO_Servlet` represents the component that will handle
   --  an HTTP request received by the server.
   type SEO_Servlet is new Servlet.Core.Servlet with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out SEO_Servlet;
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
   procedure Do_Get (Server   : in SEO_Servlet;
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class);

private

   type SEO_Servlet is new Servlet.Core.Servlet with record
      Sitemap_Prefix : UString;
   end record;

end AWA.SEO.Servlets;
