-----------------------------------------------------------------------
--  awa-blogs-servlets -- Serve files saved in the storage service
--  Copyright (C) 2017, 2019, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Calendar;

with ADO;

with ASF.Requests;
with AWA.Storages.Servlets;
package AWA.Blogs.Servlets is

   --  The <b>Storage_Servlet</b> represents the component that will handle
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

   --  Get the expected return mode (content disposition for download or inline).
   overriding
   function Get_Format (Server   : in Image_Servlet;
                        Request  : in ASF.Requests.Request'Class)
                        return AWA.Storages.Servlets.Get_Type;

private

   type Image_Servlet is new AWA.Storages.Servlets.Storage_Servlet with null record;

end AWA.Blogs.Servlets;
