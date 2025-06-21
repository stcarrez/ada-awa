-----------------------------------------------------------------------
--  awa-images-servlets -- Serve files saved in the storage service
--  Copyright (C) 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body AWA.Images.Servlets is

   --  ------------------------------
   --  Load the data content that correspond to the GET request and get the name as well
   --  as mime-type and date.
   --  ------------------------------
   overriding
   procedure Load (Server   : in Image_Servlet;
                   Request  : in out ASF.Requests.Request'Class;
                   Name     : out Ada.Strings.Unbounded.Unbounded_String;
                   Mime     : out Ada.Strings.Unbounded.Unbounded_String;
                   Date     : out Ada.Calendar.Time;
                   Data     : out ADO.Blob_Ref) is
   begin
      AWA.Storages.Servlets.Storage_Servlet (Server).Load (Request, Name, Mime, Date, Data);
      Name := Ada.Strings.Unbounded.To_Unbounded_String ("");
   end Load;

end AWA.Images.Servlets;
