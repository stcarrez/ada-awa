-----------------------------------------------------------------------
--  awa-mail-components-factory -- Mail UI Component Factory
--  Copyright (C) 2012, 2020 Stephane Carrez
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
with ASF.Views.Nodes;
with ASF.Components.Base;

with AWA.Mail.Modules;
with AWA.Mail.Components.Messages;
with AWA.Mail.Components.Recipients;
with AWA.Mail.Components.Attachments;

package body AWA.Mail.Components.Factory is

   use ASF.Components.Base;
   use ASF.Views.Nodes;

   function Create_Bcc return UIComponent_Access;
   function Create_Body return UIComponent_Access;
   function Create_Cc return UIComponent_Access;
   function Create_From return UIComponent_Access;
   function Create_Message return UIComponent_Access;
   function Create_To return UIComponent_Access;
   function Create_Subject return UIComponent_Access;
   function Create_Attachment return UIComponent_Access;

   URI                : aliased constant String := "http://code.google.com/p/ada-awa/mail";
   BCC_TAG            : aliased constant String := "bcc";
   BODY_TAG           : aliased constant String := "body";
   CC_TAG             : aliased constant String := "cc";
   FROM_TAG           : aliased constant String := "from";
   MESSAGE_TAG        : aliased constant String := "message";
   SUBJECT_TAG        : aliased constant String := "subject";
   TO_TAG             : aliased constant String := "to";
   ATTACHMENT_TAG     : aliased constant String := "attachment";

   --  ------------------------------
   --  Create an UIMailRecipient component
   --  ------------------------------
   function Create_Bcc return UIComponent_Access is
      Result : constant Recipients.UIMailRecipient_Access := new Recipients.UIMailRecipient;
   begin
      Result.Set_Recipient (AWA.Mail.Clients.BCC);
      return Result.all'Access;
   end Create_Bcc;

   --  ------------------------------
   --  Create an UIMailBody component
   --  ------------------------------
   function Create_Body return UIComponent_Access is
   begin
      return new Messages.UIMailBody;
   end Create_Body;

   --  ------------------------------
   --  Create an UIMailRecipient component
   --  ------------------------------
   function Create_Cc return UIComponent_Access is
      Result : constant Recipients.UIMailRecipient_Access := new Recipients.UIMailRecipient;
   begin
      Result.Set_Recipient (AWA.Mail.Clients.CC);
      return Result.all'Access;
   end Create_Cc;

   --  ------------------------------
   --  Create an UISender component
   --  ------------------------------
   function Create_From return UIComponent_Access is
   begin
      return new Recipients.UISender;
   end Create_From;

   --  ------------------------------
   --  Create an UIMailMessage component
   --  ------------------------------
   function Create_Message return UIComponent_Access is
      use type AWA.Mail.Modules.Mail_Module_Access;

      Result : constant Messages.UIMailMessage_Access := new Messages.UIMailMessage;
      Module : constant AWA.Mail.Modules.Mail_Module_Access := AWA.Mail.Modules.Get_Mail_Module;
   begin
      if Module = null then
         return null;
      end if;
      Result.Set_Message (Module.Create_Message);
      return Result.all'Access;
   end Create_Message;

   --  ------------------------------
   --  Create an UIMailSubject component
   --  ------------------------------
   function Create_Subject return UIComponent_Access is
   begin
      return new Messages.UIMailSubject;
   end Create_Subject;

   --  ------------------------------
   --  Create an UIMailRecipient component
   --  ------------------------------
   function Create_To return UIComponent_Access is
   begin
      return new Recipients.UIMailRecipient;
   end Create_To;

   --  ------------------------------
   --  Create an UIMailAttachment component
   --  ------------------------------
   function Create_Attachment return UIComponent_Access is
   begin
      return new Attachments.UIMailAttachment;
   end Create_Attachment;

   Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => ATTACHMENT_TAG'Access,
               Component => Create_Attachment'Access,
               Tag       => Create_Component_Node'Access),
         2 => (Name      => BCC_TAG'Access,
               Component => Create_Bcc'Access,
               Tag       => Create_Component_Node'Access),
         3 => (Name      => BODY_TAG'Access,
               Component => Create_Body'Access,
               Tag       => Create_Component_Node'Access),
         4 => (Name      => CC_TAG'Access,
               Component => Create_Cc'Access,
               Tag       => Create_Component_Node'Access),
         5 => (Name      => FROM_TAG'Access,
               Component => Create_From'Access,
               Tag       => Create_Component_Node'Access),
         6 => (Name      => MESSAGE_TAG'Access,
               Component => Create_Message'Access,
               Tag       => Create_Component_Node'Access),
         7 => (Name      => SUBJECT_TAG'Access,
               Component => Create_Subject'Access,
               Tag       => Create_Component_Node'Access),
         8 => (Name      => TO_TAG'Access,
               Component => Create_To'Access,
               Tag       => Create_Component_Node'Access)
        );

   Mail_Factory : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => Bindings'Access);

   --  ------------------------------
   --  Get the AWA Mail component factory.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return Mail_Factory'Access;
   end Definition;

end AWA.Mail.Components.Factory;
