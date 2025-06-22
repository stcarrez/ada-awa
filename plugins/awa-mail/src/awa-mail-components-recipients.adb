-----------------------------------------------------------------------
--  awa-mail-components-recipients -- Mail UI Recipients
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Objects;

package body AWA.Mail.Components.Recipients is

   --  ------------------------------
   --  Set the recipient type.
   --  ------------------------------
   procedure Set_Recipient (UI        : in out UIMailRecipient;
                            Recipient : in AWA.Mail.Clients.Recipient_Type) is
   begin
      UI.Recipient := Recipient;
   end Set_Recipient;

   --  ------------------------------
   --  Get the mail name.
   --  ------------------------------
   function Get_Name (UI      : in UIMailRecipient;
                      Context : in ASF.Contexts.Faces.Faces_Context'Class) return String is
      Name : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "name");
   begin
      if Util.Beans.Objects.Is_Null (Name) then
         return "";
      else
         return Util.Beans.Objects.To_String (Name);
      end if;
   end Get_Name;

   --  ------------------------------
   --  Get the mail address.
   --  ------------------------------
   function Get_Address (UI      : in UIMailRecipient;
                         Context : in ASF.Contexts.Faces.Faces_Context'Class) return String is
      Addr : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "address");
   begin
      if Util.Beans.Objects.Is_Null (Addr) then
         return "";
      else
         return Util.Beans.Objects.To_String (Addr);
      end if;
   end Get_Address;

   --  ------------------------------
   --  Render the children components to obtain an email address and set the TO/CC/BCC address
   --  of the message with the result.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIMailRecipient;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      procedure Process (Content : in String);

      procedure Process (Content : in String) is
         Msg  : constant AWA.Mail.Clients.Mail_Message_Access := UI.Get_Message;
         Name : constant String := UI.Get_Name (Context);
      begin
         if Content'Length > 0 then
            Msg.Add_Recipient (Kind    => UI.Recipient,
                               Name    => Name,
                               Address => Content);
         else
            Msg.Add_Recipient (Kind    => UI.Recipient,
                               Name    => Name,
                               Address => UI.Get_Address (Context));
         end if;
      end Process;

   begin
      UI.Wrap_Encode_Children (Context, Process'Access);
   end Encode_Children;

   --  ------------------------------
   --  Render the children components to obtain an email address and set the From address
   --  of the message with the result.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UISender;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      procedure Process (Content : in String);

      procedure Process (Content : in String) is
         Msg  : constant AWA.Mail.Clients.Mail_Message_Access := UI.Get_Message;
         Name : constant String := UI.Get_Name (Context);
      begin
         if Content'Length > 0 then
            Msg.Set_From (Name    => Name,
                          Address => Content);
         else
            Msg.Set_From (Name    => Name,
                          Address => UI.Get_Address (Context));
         end if;
      end Process;

   begin
      UI.Wrap_Encode_Children (Context, Process'Access);
   end Encode_Children;

end AWA.Mail.Components.Recipients;
