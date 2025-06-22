-----------------------------------------------------------------------
--  awa-votes-beans -- Beans for module votes
--  Copyright (C) 2013, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with AWA.Helpers.Requests;
package body AWA.Votes.Beans is

   --  ------------------------------
   --  Action to vote up.
   --  ------------------------------
   overriding
   procedure Vote_Up (Bean    : in out Vote_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Entity_Id := AWA.Helpers.Requests.Get_Parameter ("id");
      Bean.Rating    := 1;
      Bean.Module.Vote_For (Permission  => Ada.Strings.Unbounded.To_String (Bean.Permission),
                            Entity_Type => Ada.Strings.Unbounded.To_String (Bean.Entity_Type),
                            Id          => Bean.Entity_Id,
                            Value       => Bean.Rating,
                            Total       => Bean.Total);
   end Vote_Up;

   --  ------------------------------
   --  Action to vote down.
   --  ------------------------------
   overriding
   procedure Vote_Down (Bean    : in out Vote_Bean;
                        Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Entity_Id := AWA.Helpers.Requests.Get_Parameter ("id");
      Bean.Rating    := -1;
      Bean.Module.Vote_For (Permission  => Ada.Strings.Unbounded.To_String (Bean.Permission),
                            Entity_Type => Ada.Strings.Unbounded.To_String (Bean.Entity_Type),
                            Id          => Bean.Entity_Id,
                            Value       => Bean.Rating,
                            Total       => Bean.Total);
   end Vote_Down;

   --  ------------------------------
   --  Action to vote.
   --  ------------------------------
   overriding
   procedure Vote (Bean    : in out Vote_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Entity_Id := AWA.Helpers.Requests.Get_Parameter ("id");
      Bean.Rating    := AWA.Helpers.Requests.Get_Parameter ("rating", 0);
      if Bean.Rating /= 0 and then Bean.Rating /= -1 and then Bean.Rating /= 1 then
         Bean.Rating := 0;
      end if;
      Bean.Module.Vote_For (Permission  => Ada.Strings.Unbounded.To_String (Bean.Permission),
                            Entity_Type => Ada.Strings.Unbounded.To_String (Bean.Entity_Type),
                            Id          => Bean.Entity_Id,
                            Value       => Bean.Rating,
                            Total       => Bean.Total);
   end Vote;

   --  ------------------------------
   --  Create the Vote_Bean bean instance.
   --  ------------------------------
   function Create_Vote_Bean (Module : in AWA.Votes.Modules.Vote_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Vote_Bean_Access := new Vote_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Vote_Bean;

end AWA.Votes.Beans;
