-----------------------------------------------------------------------
--  awa-services -- Services
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Util.Strings;
package AWA.Services is

   pragma Preelaborate;

   subtype Service_Id is Util.Strings.Name_Access;

   type Service is limited interface;
   type Service_Access is access all Service'Class;

   package Service_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Service_Id,
      Element_Type    => Service_Access,
      Hash            => Util.Strings.Hash,
      Equivalent_Keys => Util.Strings.Equivalent_Keys);

end AWA.Services;
