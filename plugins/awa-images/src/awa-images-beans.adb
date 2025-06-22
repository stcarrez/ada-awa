-----------------------------------------------------------------------
--  awa-images-beans -- Image Ada Beans
--  Copyright (C) 2016, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Queries;
with ADO.Sessions;
with ADO.Objects;

with AWA.Services.Contexts;
with AWA.Storages.Modules;
package body AWA.Images.Beans is

   package ASC renames AWA.Services.Contexts;

   --  ------------------------------
   --  Load the list of images associated with the current folder.
   --  ------------------------------
   overriding
   procedure Load_Files (Storage : in out Image_List_Bean) is
      Ctx       : constant ASC.Service_Context_Access := ASC.Current;
      User      : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session   : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query     : ADO.Queries.Context;
   begin
      if not Storage.Init_Flags (AWA.Storages.Beans.INIT_FOLDER) then
         Load_Folder (Storage);
      end if;
      Query.Set_Query (AWA.Images.Models.Query_Image_List);
      Query.Bind_Param ("user_id", User);
      if Storage.Folder_Bean.Is_Null then
         Query.Bind_Null_Param ("folder_id");
      else
         Query.Bind_Param ("folder_id", Storage.Folder_Bean.Get_Id);
      end if;
      AWA.Images.Models.List (Storage.Image_List_Bean.all, Session, Query);
      Storage.Flags (AWA.Storages.Beans.INIT_FILE_LIST) := True;
   end Load_Files;

   overriding
   function Get_Value (List : in Image_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "images" then
         return Util.Beans.Objects.To_Object (Value   => List.Image_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "folders" then
         return Util.Beans.Objects.To_Object (Value   => List.Folder_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "folder" then
--           if not List.Init_Flags (INIT_FOLDER) then
--              Load_Folder (List);
--           end if;
         if List.Folder_Bean.Is_Null then
            return Util.Beans.Objects.Null_Object;
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Folder_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Create the Image_List_Bean bean instance.
   --  ------------------------------
   function Create_Image_List_Bean (Module : in AWA.Images.Modules.Image_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);

      Object    : constant Image_List_Bean_Access := new Image_List_Bean;
   begin
      Object.Module           := AWA.Storages.Modules.Get_Storage_Module;
      Object.Folder_Bean      := Object.Folder'Access;
      Object.Folder_List_Bean := Object.Folder_List'Access;
      Object.Files_List_Bean  := Object.Files_List'Access;
      Object.Image_List_Bean  := Object.Image_List'Access;
      Object.Flags            := Object.Init_Flags'Access;
      return Object.all'Access;
   end Create_Image_List_Bean;

   overriding
   procedure Load (Into    : in out Image_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type ADO.Identifier;

      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
   begin
      if Into.Id = ADO.NO_IDENTIFIER then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
         return;
      end if;

      --  Get the image information.
      Query.Set_Query (AWA.Images.Models.Query_Image_Info);
      Query.Bind_Param (Name => "user_id", Value => User);
      Query.Bind_Param (Name => "file_id", Value => Into.Id);
      Into.Load (Session, Query);

   exception
      when ADO.Objects.NOT_FOUND =>
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");

   end Load;

   --  ------------------------------
   --  Create the Image_Bean bean instance.
   --  ------------------------------
   function Create_Image_Bean (Module : in AWA.Images.Modules.Image_Module_Access)
                               return Util.Beans.Basic.Readonly_Bean_Access is
      Object    : constant Image_Bean_Access := new Image_Bean;
   begin
      Object.Module  := Module;
      Object.Id      := ADO.NO_IDENTIFIER;
      return Object.all'Access;
   end Create_Image_Bean;

end AWA.Images.Beans;
