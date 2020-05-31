-----------------------------------------------------------------------
--  awa-mail-components-attachments -- Mail UI Attachments
--  Copyright (C) 2020 Stephane Carrez
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
