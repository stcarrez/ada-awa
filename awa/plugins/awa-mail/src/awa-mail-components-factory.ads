-----------------------------------------------------------------------
--  awa-mail-components-factory -- Mail UI Component Factory
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Factory;

package AWA.Mail.Components.Factory is

   --  Get the AWA Mail component factory.
   function Definition return ASF.Factory.Factory_Bindings_Access;

end AWA.Mail.Components.Factory;
