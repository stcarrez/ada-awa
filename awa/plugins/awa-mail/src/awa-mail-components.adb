-----------------------------------------------------------------------
--  awa-mail-components -- Mail UI Components
--  Copyright (C) 2012 Stephane Carrez
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
