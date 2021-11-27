-----------------------------------------------------------------------
--  AWA.Sysadmin.Models -- AWA.Sysadmin.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.2.3
-----------------------------------------------------------------------
--  Copyright (C) 2021 Stephane Carrez
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
pragma Warnings (Off);
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with ADO.Queries;
with ADO.Queries.Loaders;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Basic.Lists;
with Util.Beans.Methods;
pragma Warnings (On);
package AWA.Sysadmin.Models is

   pragma Style_Checks ("-mr");



   Query_Sysadmin_User_List : constant ADO.Queries.Query_Definition_Access;


   Query_Sysadmin_Job_List : constant ADO.Queries.Query_Definition_Access;


   Query_Sysadmin_Session_List : constant ADO.Queries.Query_Definition_Access;


   --  --------------------
   --    authenticate the sysadmin user
   --  --------------------
   type Authenticate_Bean is abstract limited
     new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with  record

      --  the password.
      Password : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Authenticate_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Authenticate_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Authenticate_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Authenticate (Bean : in out Authenticate_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;


private

   package File_1 is
      new ADO.Queries.Loaders.File (Path => "sysadmin-users.xml",
                                    Sha1 => "9B2B599473F75F92CB5AB5045675E4CCEF926543");

   package Def_Sysadmin_User_List is
      new ADO.Queries.Loaders.Query (Name => "sysadmin-user-list",
                                     File => File_1.File'Access);
   Query_Sysadmin_User_List : constant ADO.Queries.Query_Definition_Access
   := Def_Sysadmin_User_List.Query'Access;

   package File_2 is
      new ADO.Queries.Loaders.File (Path => "sysadmin-jobs.xml",
                                    Sha1 => "9B2B599473F75F92CB5AB5045675E4CCEF926543");

   package Def_Sysadmin_Job_List is
      new ADO.Queries.Loaders.Query (Name => "sysadmin-job-list",
                                     File => File_2.File'Access);
   Query_Sysadmin_Job_List : constant ADO.Queries.Query_Definition_Access
   := Def_Sysadmin_Job_List.Query'Access;

   package File_3 is
      new ADO.Queries.Loaders.File (Path => "sysadmin-sessions.xml",
                                    Sha1 => "9B2B599473F75F92CB5AB5045675E4CCEF926543");

   package Def_Sysadmin_Session_List is
      new ADO.Queries.Loaders.Query (Name => "sysadmin-session-list",
                                     File => File_3.File'Access);
   Query_Sysadmin_Session_List : constant ADO.Queries.Query_Definition_Access
   := Def_Sysadmin_Session_List.Query'Access;
end AWA.Sysadmin.Models;
