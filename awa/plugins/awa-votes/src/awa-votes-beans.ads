-----------------------------------------------------------------------
--  awa-votes-beans -- Beans for module votes
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

with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with AWA.Votes.Services;
with AWA.Votes.Modules;
with AWA.Votes.Models;
package AWA.Votes.Beans is

   type Vote_Bean is new AWA.Votes.Models.Vote_Bean with record
      Service : AWA.Votes.Services.Vote_Service_Access := null;
   end record;
   type Vote_Bean_Access is access all Vote_Bean'Class;

   --  Action to vote up.
   overriding
   procedure Vote_Up (Bean    : in out Vote_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Action to vote down.
   overriding
   procedure Vote_Down (Bean    : in out Vote_Bean;
                        Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Votes_Bean bean instance.
   function Create_Vote_Bean (Module : in AWA.Votes.Modules.Vote_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Votes.Beans;
