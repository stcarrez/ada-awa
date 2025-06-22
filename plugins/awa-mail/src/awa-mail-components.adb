-----------------------------------------------------------------------
--  awa-mail-components -- Mail UI Components
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Components.Base;

with AWA.Mail.Components.Messages;

package body AWA.Mail.Components is

   --  ------------------------------
   --  Get the mail message instance.
   --  ------------------------------
   function Get_Message (UI : in UIMailComponent) return AWA.Mail.Clients.Mail_Message_Access is
      use type ASF.Components.Base.UIComponent_Access;

      Parent : ASF.Components.Base.UIComponent_Access := UI.Get_Parent;
   begin
      while Parent /= null loop
         if Parent.all in AWA.Mail.Components.Messages.UIMailMessage'Class then
            return AWA.Mail.Components.Messages.UIMailMessage'Class (Parent.all).Get_Message;
         end if;
         Parent := Parent.Get_Parent;
      end loop;

      return null;
   end Get_Message;

end AWA.Mail.Components;
