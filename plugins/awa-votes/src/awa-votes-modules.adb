-----------------------------------------------------------------------
--  awa-votes-modules -- Module votes
--  Copyright (C) 2013, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Log.Loggers;

with Security.Permissions;

with AWA.Modules.Beans;
with AWA.Modules.Get;
with AWA.Votes.Beans;
with AWA.Permissions;
with AWA.Services.Contexts;
with AWA.Votes.Models;

with ADO.SQL;
with ADO.Sessions;
with ADO.Sessions.Entities;

package body AWA.Votes.Modules is

   use AWA.Services;
   use ADO.Sessions;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Votes.Module");

   package Register is new AWA.Modules.Beans (Module => Vote_Module,
                                              Module_Access => Vote_Module_Access);

   --  ------------------------------
   --  Initialize the votes module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Vote_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the votes module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "AWA.Votes.Beans.Votes_Bean",
                         Handler => AWA.Votes.Beans.Create_Vote_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the votes module.
   --  ------------------------------
   function Get_Vote_Module return Vote_Module_Access is
      function Get is new AWA.Modules.Get (Vote_Module, Vote_Module_Access, NAME);
   begin
      return Get;
   end Get_Vote_Module;

   --  ------------------------------
   --  Vote for the given element and return the total vote for that element.
   --  ------------------------------
   procedure Vote_For (Model       : in Vote_Module;
                       Id          : in ADO.Identifier;
                       Entity_Type : in String;
                       Permission  : in String;
                       Value       : in Integer;
                       Total       : out Integer) is
      pragma Unreferenced (Model);

      Ctx    : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User   : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB     : Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      Kind   : ADO.Entity_Type;
      Rating : AWA.Votes.Models.Rating_Ref;
      Vote   : AWA.Votes.Models.Vote_Ref;
      Query  : ADO.SQL.Query;
      Found  : Boolean;
      Ident  : constant String := Entity_Type & ADO.Identifier'Image (Id);
   begin
      --  Check that the user has the vote permission on the given object.
      AWA.Permissions.Check (Permission => Security.Permissions.Get_Permission_Index (Permission),
                             Entity     => Id);

      Log.Info ("User {0} votes for {1} rating {2}",
                ADO.Identifier'Image (User), Ident,
                Integer'Image (Value));

      Ctx.Start;
      Kind := ADO.Sessions.Entities.Find_Entity_Type (DB, Entity_Type);

      --  Get the vote associated with the object for the user.
      Query.Set_Join ("INNER JOIN awa_rating r ON r.id = o.entity_id");
      Query.Set_Filter ("r.for_entity_id = :id and r.for_entity_type = :type "
                        & "and o.user_id = :user_id");
      Query.Bind_Param ("id", Id);
      Query.Bind_Param ("type", Kind);
      Query.Bind_Param ("user_id", User);
      Vote.Find (DB, Query, Found);
      if not Found then
         Query.Clear;

         --  Get the rating associated with the object.
         Query.Set_Filter ("for_entity_id = :id and for_entity_type = :type");
         Query.Bind_Param ("id", Id);
         Query.Bind_Param ("type", Kind);
         Rating.Find (DB, Query, Found);

         --  Create it if it does not exist.
         if not Found then
            Log.Info ("Creating rating for {0}", Ident);

            Rating.Set_For_Entity_Id (Id);
            Rating.Set_For_Entity_Type (Kind);
            Rating.Set_Rating (Value);
            Rating.Set_Vote_Count (1);
         else
            Rating.Set_Vote_Count (Rating.Get_Vote_Count + 1);
            Rating.Set_Rating (Value + Rating.Get_Rating);
         end if;
         Rating.Save (DB);

         Vote.Set_User_Id (User);
         Vote.Set_Entity (Rating);
      else
         Rating := AWA.Votes.Models.Rating_Ref (Vote.Get_Entity);
         Rating.Set_Rating (Rating.Get_Rating + Value - Vote.Get_Rating);
         Rating.Save (DB);
      end if;
      Vote.Set_Rating (Value);
      Vote.Save (DB);

      --  Return the total rating for the element.
      Total := Rating.Get_Rating;
      Ctx.Commit;
   end Vote_For;

end AWA.Votes.Modules;
