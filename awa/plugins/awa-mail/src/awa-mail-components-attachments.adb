-----------------------------------------------------------------------
--  awa-mail-components-attachments -- Mail UI Attachments
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
package body AWA.Mail.Components.Attachments is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Render the mail subject and initializes the message with its content.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIMailAttachment;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      procedure Process (Content : in Unbounded_String);

      Content_Type : Util.Beans.Objects.Object;
      File_Name    : Util.Beans.Objects.Object;
      Value        : Util.Beans.Objects.Object;
      Msg          : constant AWA.Mail.Clients.Mail_Message_Access := UI.Get_Message;

      function Get_Content_Id return String is
        (if not Util.Beans.Objects.Is_Empty (File_Name) then
            Util.Beans.Objects.To_String (File_Name)
         else
            To_String (UI.Get_Client_Id));

      function Get_Content_Type return String is
        (if Util.Beans.Objects.Is_Empty (Content_Type) then
              ""
         else
            Util.Beans.Objects.To_String (Content_Type));

      procedure Process (Content : in Unbounded_String) is
      begin
         Msg.Add_Attachment (Content, Get_Content_Id, Get_Content_Type);
      end Process;

   begin
      Content_Type := UI.Get_Attribute (Name => "contentType", Context => Context);
      File_Name := UI.Get_Attribute (Name => "fileName", Context => Context);
      Value := UI.Get_Attribute (Name => "value", Context => Context);
      if Util.Beans.Objects.Is_Empty (Value) then
         UI.Wrap_Encode_Children (Context, Process'Access);
      else
         Msg.Add_File_Attachment (Util.Beans.Objects.To_String (Value),
                                  Get_Content_Id,
                                  Get_Content_Type);
      end if;
   end Encode_Children;

end AWA.Mail.Components.Attachments;
