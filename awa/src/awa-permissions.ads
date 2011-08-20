-----------------------------------------------------------------------
--  awa-permissions -- Permissions module
--  Copyright (C) 2011 Stephane Carrez
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

with Security.Permissions;

with ADO;
package AWA.Permissions is

   --  Exception raised by the <b>Check</b> procedure if the user does not have
   --  the permission.
   NO_PERMISSION : exception;

   type Permission_Type is (READ, WRITE);

   --  Verify that the permission represented by <b>Permission</b> is granted.
   --
   procedure Check (Permission : in Security.Permissions.Permission_Index);

   --  Verify that the permission represented by <b>Permission</b> is granted to access the
   --  database entity represented by <b>Entity</b>.
   procedure Check (Permission : in Security.Permissions.Permission_Index;
                    Entity     : in ADO.Identifier);

end AWA.Permissions;
