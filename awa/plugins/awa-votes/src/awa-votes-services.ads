-----------------------------------------------------------------------
--  awa-votes-services -- Service Vote
--  Copyright (C) 2013 Stephane Carrez
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

with ADO;
with ADO.Objects;

with AWA.Modules;
with Security.Permissions;
package AWA.Votes.Services is

   --  ------------------------------
   --  Service Vote
   --  ------------------------------
   type Vote_Service is new AWA.Modules.Module_Manager with private;
   type Vote_Service_Access is access all Vote_Service'Class;

   --  Set the permission to be used by the service.
   procedure Set_Permission (Service    : in out Vote_Service;
                             Permission : in Security.Permissions.Permission_Index);

   --  Vote for the given element.
   procedure Vote_For (Model  : in Vote_Service;
                       Object : in ADO.Objects.Object_Ref'Class;
                       Rating : in Integer);

private

   type Vote_Service is new AWA.Modules.Module_Manager with record
      Vote_Permission : Security.Permissions.Permission_Index;
   end record;

end AWA.Votes.Services;
