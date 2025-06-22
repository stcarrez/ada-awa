-----------------------------------------------------------------------
--  awa-components-factory -- Factory for AWA UI Components
--  Copyright (C) 2011, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Factory;
package AWA.Components.Factory is

   --  Get the AWA component factory.
   function Definition return ASF.Factory.Factory_Bindings_Access;

end AWA.Components.Factory;
