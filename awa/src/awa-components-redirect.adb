-----------------------------------------------------------------------
--  awa-components-redirect -- ASF Core Components
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body AWA.Components.Redirect is

   --  ------------------------------
   --  Get the redirection link
   --  ------------------------------
   function Get_Link (UI      : in UIRedirect;
                      Context : in Faces_Context'Class) return Util.Beans.Objects.Object is
   begin
      return UI.Get_Attribute (Context, "link");
   end Get_Link;

   --  ------------------------------
   --  If the component is rendered, activate the redirection to the
   --  specified URI.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIRedirect;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Link : constant Util.Beans.Objects.Object := UIRedirect'Class (UI).Get_Link (Context);
      begin
         Context.Get_Response.Send_Redirect (Location => Util.Beans.Objects.To_String (Link));
      end;
   end Encode_Begin;

end AWA.Components.Redirect;
