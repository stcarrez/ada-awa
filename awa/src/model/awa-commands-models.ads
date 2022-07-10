-----------------------------------------------------------------------
--  AWA.Commands.Models -- AWA.Commands.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.2.3
-----------------------------------------------------------------------
--  Copyright (C) 2022 Stephane Carrez
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
pragma Warnings (On);
package AWA.Commands.Models is

   pragma Style_Checks ("-mrIu");



   Query_Command_User_List : constant ADO.Queries.Query_Definition_Access;


   Query_Command_Job_List : constant ADO.Queries.Query_Definition_Access;


   Query_Command_Session_List : constant ADO.Queries.Query_Definition_Access;



private

   package File_1 is
      new ADO.Queries.Loaders.File (Path => "command-users.xml",
                                    Sha1 => "9B2B599473F75F92CB5AB5045675E4CCEF926543");

   package Def_Command_User_List is
      new ADO.Queries.Loaders.Query (Name => "command-user-list",
                                     File => File_1.File'Access);
   Query_Command_User_List : constant ADO.Queries.Query_Definition_Access
   := Def_Command_User_List.Query'Access;

   package File_2 is
      new ADO.Queries.Loaders.File (Path => "command-jobs.xml",
                                    Sha1 => "9B2B599473F75F92CB5AB5045675E4CCEF926543");

   package Def_Command_Job_List is
      new ADO.Queries.Loaders.Query (Name => "command-job-list",
                                     File => File_2.File'Access);
   Query_Command_Job_List : constant ADO.Queries.Query_Definition_Access
   := Def_Command_Job_List.Query'Access;

   package File_3 is
      new ADO.Queries.Loaders.File (Path => "command-sessions.xml",
                                    Sha1 => "9B2B599473F75F92CB5AB5045675E4CCEF926543");

   package Def_Command_Session_List is
      new ADO.Queries.Loaders.Query (Name => "command-session-list",
                                     File => File_3.File'Access);
   Query_Command_Session_List : constant ADO.Queries.Query_Definition_Access
   := Def_Command_Session_List.Query'Access;
end AWA.Commands.Models;
