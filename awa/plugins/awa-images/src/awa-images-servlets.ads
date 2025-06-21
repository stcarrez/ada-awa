-----------------------------------------------------------------------
--  awa-images-servlets -- Serve images saved in the storage service
--  Copyright (C) 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Ada.Calendar;

with ASF.Requests;
with AWA.Storages.Servlets;

with ADO;
package AWA.Images.Servlets is

   --  The <b>Image_Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   type Image_Servlet is new AWA.Storages.Servlets.Storage_Servlet with private;

   --  Load the data content that correspond to the GET request and get the name as well
   --  as mime-type and date.
   overriding
   procedure Load (Server   : in Image_Servlet;
                   Request  : in out ASF.Requests.Request'Class;
                   Name     : out Ada.Strings.Unbounded.Unbounded_String;
                   Mime     : out Ada.Strings.Unbounded.Unbounded_String;
                   Date     : out Ada.Calendar.Time;
                   Data     : out ADO.Blob_Ref);

private

   type Image_Servlet is new AWA.Storages.Servlets.Storage_Servlet with null record;

end AWA.Images.Servlets;
