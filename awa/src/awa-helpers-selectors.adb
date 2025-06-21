-----------------------------------------------------------------------
--  awa-helpers -- Helpers for AWA applications
--  Copyright (C) 2011, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Queries.Loaders;
with ADO.Schemas;

with Util.Strings;
with Util.Log.Loggers;
with Util.Locales;

with ASF.Locales;

with AWA.Services.Contexts;
package body AWA.Helpers.Selectors is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Helpers.Selectors");

   --  ------------------------------
   --  Create a selector list from the definition of a discrete type such as an enum.
   --  The select item has the enum value as value and the label is created by
   --  localizing the string <b>Prefix</b>_<i>enum name</i>.
   --  ------------------------------
   function Create_From_Enum (Bundle : in Util.Properties.Bundles.Manager'Class)
                              return ASF.Models.Selects.Select_Item_List is
      Result : ASF.Models.Selects.Select_Item_List;
   begin
      for I in T'Range loop
         declare
            Value : constant String := T'Image (I);
            Name  : constant String := Prefix & "_" & Value;
            Label : constant String := Bundle.Get (Name, Name);
         begin
            Result.Append (ASF.Models.Selects.Create_Select_Item (Label, Value));
         end;
      end loop;
      return Result;
   end Create_From_Enum;

   --  ------------------------------
   --  Create a selector list by using a resource bundle and a create operation that looks for
   --  messages in the bundle.  The bundle name <b>Bundle</b> gives the name of the resource
   --  bundled to load.  The locale is determined by the ASF context passed in <b>Context</b>.
   --  The <b>Create</b> function is in charge of creating and populating the select list.
   --  ------------------------------
   function Create_Selector_Bean (Bundle  : in String;
                                  Context : in ASF.Contexts.Faces.Faces_Context_Access := null;
                                  Create  : access function
                                    (Bundle : in Util.Properties.Bundles.Manager'Class)
                                  return ASF.Models.Selects.Select_Item_List)
                                  return Util.Beans.Basic.Readonly_Bean_Access is
      use ASF.Contexts.Faces;

      Ctx     : ASF.Contexts.Faces.Faces_Context_Access;
      Manager : ASF.Locales.Bundle;
      Result  : ASF.Models.Selects.Select_Item_List_Access;
   begin
      if Context = null then
         Ctx := ASF.Contexts.Faces.Current;
      end if;
      if Ctx /= null then
         declare
            Locale : constant Util.Locales.Locale := Ctx.Get_Locale;
            App    : constant Application_Access := Ctx.Get_Application;
         begin
            App.Load_Bundle (Name   => Bundle,
                             Locale => Util.Locales.To_String (Locale),
                             Bundle => Manager);
         end;
      else
         Log.Warn ("No context defined to localize the selector");
      end if;
      Result := new ASF.Models.Selects.Select_Item_List;
      Result.all := Create (Manager);
      return Result.all'Access;
   end Create_Selector_Bean;

   --  ------------------------------
   --  Append the selector list from the SQL query.  The query will be executed.
   --  It should return rows with at least two columns.  The first column is the
   --  selector value and the second column is the selector label.
   --  ------------------------------
   procedure Append_From_Query (Into  : in out ASF.Models.Selects.Select_Item_List;
                                Query : in out ADO.Statements.Query_Statement'Class) is
      use ADO.Schemas;
      function Get_Column (Id : in Natural;
                           Of_Type : in ADO.Schemas.Column_Type) return String;

      Id_Type    : ADO.Schemas.Column_Type := T_UNKNOWN;
      Label_Type : ADO.Schemas.Column_Type := T_UNKNOWN;

      function Get_Column (Id : in Natural;
                           Of_Type : in ADO.Schemas.Column_Type) return String is
      begin
         case Of_Type is
            when T_UNKNOWN | T_CHAR | T_VARCHAR =>
               return Query.Get_String (Id);

            when T_INTEGER | T_TINYINT | T_SMALLINT | T_ENUM =>
               return Util.Strings.Image (Query.Get_Integer (Id));

            when T_LONG_INTEGER =>
               return Util.Strings.Image (Long_Long_Integer (Query.Get_Int64 (Id)));

            when others =>
               return "";

         end case;
      end Get_Column;

   begin
      Query.Execute;
      while Query.Has_Elements loop
         if Id_Type = T_UNKNOWN then
            Id_Type    := Query.Get_Column_Type (0);
            Label_Type := Query.Get_Column_Type (1);
         end if;
         declare
            Id    : constant String := Get_Column (0, Id_Type);
            Label : constant String := Get_Column (1, Label_Type);
         begin
            Into.Append (ASF.Models.Selects.Create_Select_Item (Label => Label, Value => Id));
         end;
         Query.Next;
      end loop;
   end Append_From_Query;

   --  ------------------------------
   --  Create the selector list from the SQL query.  The query will be executed.
   --  It should return rows with at least two columns.  The first column is the
   --  selector value and the second column is the selector label.
   --  ------------------------------
   function Create_From_Query (Session : in ADO.Sessions.Session'Class;
                               Query   : in ADO.Queries.Context'Class)
                               return ASF.Models.Selects.Select_Item_List is
      Stmt   : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
      Result : ASF.Models.Selects.Select_Item_List;
   begin
      Append_From_Query (Result, Stmt);
      return Result;
   end Create_From_Query;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Select_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "list" then
         return ASF.Models.Selects.To_Object (From.List);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  If the name cannot be found, the method should raise the No_Value
   --  exception.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Select_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
      use type AWA.Services.Contexts.Service_Context_Access;
      use type ADO.Queries.Query_Definition_Access;
   begin
      if Name = "query" then
         declare
            Query_Name : constant String := Util.Beans.Objects.To_String (Value);
            Ctx : constant AWA.Services.Contexts.Service_Context_Access
              := AWA.Services.Contexts.Current;
            Query_Def : constant ADO.Queries.Query_Definition_Access
              := ADO.Queries.Loaders.Find_Query (Query_Name);
            Query     : ADO.Queries.Context;
         begin
            if Ctx = null or else Query_Def = null then
               return;
            end if;

            Query.Set_Query (Query_Def);
            From.List := Create_From_Query (Session => AWA.Services.Contexts.Get_Session (Ctx),
                                            Query   => Query);
         end;
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create the select list bean instance.
   --  ------------------------------
   function Create_Select_List_Bean return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Select_List_Bean_Access := new Select_List_Bean;
   begin
      return Result.all'Access;
   end Create_Select_List_Bean;

end AWA.Helpers.Selectors;
