-----------------------------------------------------------------------
--  awa-seo-servlets -- Servlets to produce sitemaps
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Calendar;

with Util.Http.Mimes;
with Util.Log.Loggers;
with Util.Serialize.IO.XML;
with Util.Dates.ISO8601;

with ADO.Objects;

with Servlet.Routes;
with Servlet.Streams.XML;

with AWA.SEO.Modules;
package body AWA.SEO.Servlets is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.SEO.Servlets");

   function Format_Date (Date : in Ada.Calendar.Time) return String;

   function Format_Date (Date : in Ada.Calendar.Time) return String is
   begin
      return Util.Dates.ISO8601.Image (Date, Util.Dates.ISO8601.SECOND) & "Z";
   end Format_Date;

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   overriding
   procedure Initialize (Server  : in out SEO_Servlet;
                         Context : in Servlet.Core.Servlet_Registry'Class) is
   begin
      Server.Sitemap_Prefix
        := Context.Get_Init_Parameter ("seo." & SEO.Modules.PARAM_SITEMAP_PREFIX);
   end Initialize;

   --  ------------------------------
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
   --  ------------------------------
   overriding
   procedure Do_Get (Server   : in SEO_Servlet;
                     Request  : in out Servlet.Requests.Request'Class;
                     Response : in out Servlet.Responses.Response'Class) is
      use type Ada.Strings.Unbounded.Unbounded_String;
      URI     : constant String := Request.Get_Path_Info;
      Module  : constant AWA.SEO.Modules.SEO_Module_Access := Modules.Get_SEO_Module;
      Stream  : Servlet.Streams.XML.Print_Stream := Response.Get_Output_Stream;
   begin
      Log.Info ("GET: {0}", URI);

      Response.Set_Content_Type (Util.Http.Mimes.Xml);
      if URI'Length = 0 or else URI = "/sitemap.xml" then
         declare
            List : Sitemap_Index_Vector;
         begin
            Module.Get_Sitemap_Index (List);
            Stream.Start_Document;
            Stream.Start_Entity ("sitemapindex");
            Stream.Write_Attribute ("xmlns",
                                    "http://www.sitemaps.org/schemas/sitemap/0.9");
            for Index of List loop
               Stream.Start_Entity ("sitemap");
               Stream.Write_Entity ("loc", Server.Sitemap_Prefix & Index.Location);
               Stream.Write_Entity ("lastmod", Format_Date (Index.Date));
               Stream.End_Entity ("sitemap");
            end loop;
            Stream.End_Entity ("sitemapindex");
            Stream.End_Document;
         end;
      else
         declare
            List  : Sitemap_Entry_Vectors.Vector;
            Found : Boolean;
         begin
            Module.Get_Sitemap (URI (URI'First + 1 .. URI'Last), List, Found);
            if not Found then
               Log.Info ("GET: {0}: invalid sitemap", URI);
               Response.Send_Error (Servlet.Responses.SC_NOT_FOUND);
               return;
            end if;
            Stream.Start_Document;
            Stream.Start_Entity ("urlset");
            Stream.Write_Attribute ("xmlns",
                                    "http://www.sitemaps.org/schemas/sitemap/0.9");
            Stream.Write_Attribute ("xmlns:image",
                                    "http://www.google.com/schemas/sitemap-image/1.1");
            for Item of List loop
               Stream.Start_Entity ("url");
               Stream.Write_Entity ("loc", Item.Location);
               Stream.Write_Entity ("lastmod", Format_Date (Item.Date));
               if Length (Item.Image) > 0 then
                  Stream.Start_Entity ("image:image");
                  Stream.Write_Entity ("image:loc", Item.Image);
                  if Length (Item.Image_Title) > 0 then
                     Stream.Write_Entity ("image:title", Item.Image_Title);
                  end if;
                  Stream.End_Entity ("image:image");
               end if;
               Stream.End_Entity ("url");
            end loop;
            Stream.End_Entity ("urlset");
            Stream.End_Document;
         end;
      end if;

   exception
      when Servlet.Routes.No_Parameter =>
         Log.Info ("GET: {0}: Invalid servlet-mapping, a path parameter is missing", URI);
         Response.Send_Error (Servlet.Responses.SC_NOT_FOUND);
         return;

      when ADO.Objects.NOT_FOUND | Constraint_Error =>
         Log.Info ("GET: {0}: Storage file not found", URI);
         Response.Send_Error (Servlet.Responses.SC_NOT_FOUND);
         return;
   end Do_Get;

end AWA.SEO.Servlets;
