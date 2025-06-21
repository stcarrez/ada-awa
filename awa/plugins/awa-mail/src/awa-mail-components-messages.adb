-----------------------------------------------------------------------
--  awa-mail-components-messages -- Mail UI Message
--  Copyright (C) 2012, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

with Util.Beans.Objects;
with ASF.Components.Base;
with AWA.Mail.Clients;
package body AWA.Mail.Components.Messages is

   use Ada.Strings.Unbounded;
   use type ASF.Components.Base.UIComponent_Access;

   --  ------------------------------
   --  Set the mail message instance.
   --  ------------------------------
   procedure Set_Message (UI      : in out UIMailMessage;
                          Message : in AWA.Mail.Clients.Mail_Message_Access) is
   begin
      UI.Message := Message;
   end Set_Message;

   --  ------------------------------
   --  Get the mail message instance.
   --  ------------------------------
   overriding
   function Get_Message (UI : in UIMailMessage) return AWA.Mail.Clients.Mail_Message_Access is
   begin
      return UI.Message;
   end Get_Message;

   --  ------------------------------
   --  Render the end of the input component.  Closes the DL/DD list.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIMailMessage;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type AWA.Mail.Clients.Mail_Message_Access;
   begin
      if UI.Message /= null and then UI.Is_Rendered (Context) then
         UI.Message.Send;
      end if;
   end Encode_End;

   --  ------------------------------
   --  Finalize and release the mail message.
   --  ------------------------------
   overriding
   procedure Finalize (UI : in out UIMailMessage) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => AWA.Mail.Clients.Mail_Message'Class,
                                         Name   => AWA.Mail.Clients.Mail_Message_Access);
   begin
      Free (UI.Message);
      UIMailComponent (UI).Finalize;
   end Finalize;

   --  ------------------------------
   --  Render the mail subject and initializes the message with its content.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIMailSubject;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      procedure Process (Content : in String);

      procedure Process (Content : in String) is
         Msg : constant AWA.Mail.Clients.Mail_Message_Access := UI.Get_Message;
      begin
         Msg.Set_Subject (Content);
      end Process;

   begin
      UI.Wrap_Encode_Children (Context, Process'Access);
   end Encode_Children;

   --  ------------------------------
   --  Render the mail body and initializes the message with its content.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIMailBody;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      procedure Process (Content : in Unbounded_String);
      procedure Process_Alternative (Content : in Unbounded_String);

      Body_Type           : Util.Beans.Objects.Object;
      Alternative_Content : Unbounded_String;

      procedure Process_Alternative (Content : in Unbounded_String) is
      begin
         Alternative_Content := Content;
      end Process_Alternative;

      procedure Process (Content : in Unbounded_String) is
         Msg : constant AWA.Mail.Clients.Mail_Message_Access := UI.Get_Message;
         Typ : constant String := Util.Beans.Objects.To_String (Body_Type);
      begin
         if not Util.Beans.Objects.Is_Empty (Body_Type) then
            Msg.Set_Body (Content, Alternative_Content, Typ);
         else
            Msg.Set_Body (Content, Alternative_Content);
         end if;
      end Process;

      Alternative_Facet : ASF.Components.Base.UIComponent_Access;
   begin
      Body_Type := UI.Get_Attribute (Name => "type", Context => Context);
      Alternative_Facet := UI.Get_Facet (ALTERNATIVE_NAME);
      if Alternative_Facet /= null then
         Alternative_Facet.Wrap_Encode_Children (Context, Process_Alternative'Access);
      end if;
      UI.Wrap_Encode_Children (Context, Process'Access);
   end Encode_Children;

end AWA.Mail.Components.Messages;
