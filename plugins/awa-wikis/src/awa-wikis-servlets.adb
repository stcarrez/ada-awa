-----------------------------------------------------------------------
--  awa-wikis-servlets -- Serve files saved in the storage service
--  Copyright (C) 2016, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Images.Modules;
with AWA.Wikis.Modules;
package body AWA.Wikis.Servlets is

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
      pragma Unreferenced (Server, Name);

      Wiki      : constant String := Request.Get_Path_Parameter (1);
      File      : constant String := Request.Get_Path_Parameter (2);
      Size      : constant String := Request.Get_Path_Parameter (3);
      Module    : constant AWA.Wikis.Modules.Wiki_Module_Access := Wikis.Modules.Get_Wiki_Module;
      Wiki_Id   : ADO.Identifier;
      File_Id   : ADO.Identifier;
      Width     : Natural;
      Height    : Natural;
      Img_Width : Natural;
      Img_Height : Natural;
   begin
      Wiki_Id := ADO.Identifier'Value (Wiki);
      File_Id := ADO.Identifier'Value (File);

      AWA.Images.Modules.Get_Sizes (Dimension => Size,
                                    Width     => Width,
                                    Height    => Height);

      Img_Width  := Width;
      Img_Height := Height;
      Module.Load_Image (Wiki_Id  => Wiki_Id,
                         Image_Id => File_Id,
                         Width    => Img_Width,
                         Height   => Img_Height,
                         Mime     => Mime,
                         Date     => Date,
                         Into     => Data);

   end Load;

   --  ------------------------------
   --  Get the expected return mode (content disposition for download or inline).
   --  ------------------------------
   overriding
   function Get_Format (Server   : in Image_Servlet;
                        Request  : in ASF.Requests.Request'Class)
                        return AWA.Storages.Servlets.Get_Type is
      pragma Unreferenced (Server, Request);
   begin
      return AWA.Storages.Servlets.DEFAULT;
   end Get_Format;

end AWA.Wikis.Servlets;
