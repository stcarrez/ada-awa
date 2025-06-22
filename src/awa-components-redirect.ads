-----------------------------------------------------------------------
--  awa-components-redirect -- ASF Core Components
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Components.Core;
with ASF.Contexts.Faces;
with Util.Beans.Objects;
package AWA.Components.Redirect is

   use ASF.Contexts.Faces;

   --  ------------------------------
   --  Redirect component
   --  ------------------------------
   type UIRedirect is new ASF.Components.Core.UILeaf with null record;
   type UIRedirect_Access is access all UIRedirect'Class;

   --  Get the redirection link
   function Get_Link (UI      : in UIRedirect;
                      Context : in Faces_Context'Class) return Util.Beans.Objects.Object;

   --  If the component is rendered, activate the redirection to the
   --  specified URI.
   overriding
   procedure Encode_Begin (UI      : in UIRedirect;
                           Context : in out Faces_Context'Class);

end AWA.Components.Redirect;
