-----------------------------------------------------------------------
--  awa-votes-beans -- Beans for module votes
--  Copyright (C) 2013, 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with AWA.Votes.Modules;
with AWA.Votes.Models;

--  == Ada Beans ==
--  The `Vote_Bean` is a bean intended to be used in presentation files (XHTML facelet
--  files) to vote for an item.  The managed bean can be easily configured in the application XML
--  configuration file.  The `permission` and `entity_type` are the two properties
--  that should be defined in the configuration.  The `permission` is the name of the
--  permission that must be used to verify that the user is allowed to vote for the item.
--  The `entity_type` is the name of the entity (table name) used by the item.
--  The example below defines the bean `questionVote` defined by the question module.
--
--    <managed-bean>
--      <description>The vote bean that allows to vote for a question.</description>
--      <managed-bean-name>questionVote</managed-bean-name>
--      <managed-bean-class>AWA.Votes.Beans.Votes_Bean</managed-bean-class>
--      <managed-bean-scope>request</managed-bean-scope>
--      <managed-property>
--        <property-name>permission</property-name>
--        <property-class>String</property-class>
--        <value>answer-create</value>
--      </managed-property>
--      <managed-property>
--        <property-name>entity_type</property-name>
--        <property-class>String</property-class>
--        <value>awa_question</value>
--      </managed-property>
--    </managed-bean>
--
--  The vote concerns entities for the `awa_question` entity table.
--  The permission `answer-create` is used to verify that the vote is allowed.
--
--  [images/awa_votes_bean.png]
--
--  The managed bean defines three operations that can be called: `vote_up`,
--  `vote_down` and `vote` to setup specific ratings.
package AWA.Votes.Beans is

   type Vote_Bean is new AWA.Votes.Models.Vote_Bean with private;
   type Vote_Bean_Access is access all Vote_Bean'Class;

   --  Action to vote up.
   overriding
   procedure Vote_Up (Bean    : in out Vote_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Action to vote down.
   overriding
   procedure Vote_Down (Bean    : in out Vote_Bean;
                        Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Action to vote.
   overriding
   procedure Vote (Bean    : in out Vote_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Votes_Bean bean instance.
   function Create_Vote_Bean (Module : in AWA.Votes.Modules.Vote_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

private

   type Vote_Bean is new AWA.Votes.Models.Vote_Bean with record
      Module : AWA.Votes.Modules.Vote_Module_Access := null;
   end record;

end AWA.Votes.Beans;
