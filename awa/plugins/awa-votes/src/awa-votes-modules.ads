-----------------------------------------------------------------------
--  awa-votes-modules -- Module votes
--  Copyright (C) 2013, 2014, 2018, 2020 Stephane Carrez
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
with ASF.Applications;
with ADO;
with AWA.Modules;

--  == Integration ==
--  To be able to use the `votes` module, you will need to add the
--  following line in your GNAT project file:
--
--    with "awa_votes";
--
--  The `Vote_Module` manages the votes on entities.  It provides operations
--  that are used by the vote beans or other services to vote for an item.
--  An instance of the `Vote_Module` must be declared and registered
--  in the AWA application.
--
--  The module instance can be defined as follows:
--
--    type Application is new AWA.Applications.Application with record
--       Vote_Module : aliased AWA.Votes.Modules.Vote_Module;
--    end record;
--
--  And registered in the `Initialize_Modules` procedure by using:
--
--    Register (App    => App.Self.all'Access,
--              Name   => AWA.Votes.Modules.NAME,
--              URI    => "votes",
--              Module => App.Vote_Module'Access);
--
package AWA.Votes.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "votes";

   --  ------------------------------
   --  Module votes
   --  ------------------------------
   type Vote_Module is new AWA.Modules.Module with private;
   type Vote_Module_Access is access all Vote_Module'Class;

   --  Initialize the votes module.
   overriding
   procedure Initialize (Plugin : in out Vote_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the votes module.
   function Get_Vote_Module return Vote_Module_Access;

   --  Vote for the given element and return the total vote for that element.
   procedure Vote_For (Model       : in Vote_Module;
                       Id          : in ADO.Identifier;
                       Entity_Type : in String;
                       Permission  : in String;
                       Value       : in Integer;
                       Total       : out Integer);

private

   type Vote_Module is new AWA.Modules.Module with null record;

end AWA.Votes.Modules;
