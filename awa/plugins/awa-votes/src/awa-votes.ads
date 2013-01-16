-----------------------------------------------------------------------
--  awa-votes -- Module votes
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

--  == Introduction ==
--  The <b>Votes</b> module allows users to vote for objects defined in the application.
--  Users can vote by setting a rating value on an item (+1, -1 or any other integer value).
--  The Votes module makes sure that users can vote only once for an item.  A global rating
--  is associated with the item to give the vote summary.  The vote can be associated with
--  any database entity and it is not necessary to change other entities data model.
--
--  == Model ==
--  [http://ada-awa.googlecode.com/svn/wiki/awa_votes_model.png]
--
--  == Integration ==
--  To add the <b>Votes</b> module in an application, the <tt>Vote_Module</tt> instance must
--  be declared and registered in the AWA application.
--
--  @include awa-votes-beans.ads
package AWA.Votes is

   pragma Preelaborate;

end AWA.Votes;
