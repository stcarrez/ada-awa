-----------------------------------------------------------------------
--  awa-questions-services -- Service services
--  Copyright (C) 2012 Stephane Carrez
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

with AWA.Modules;
with Security.Permissions;
with AWA.Questions.Models;
package AWA.Questions.Services is

   --  Define the permissions.
   package ACL_Create_Questions is new Security.Permissions.Definition ("question-create");
   package ACL_Delete_Questions is new Security.Permissions.Definition ("question-delete");
   package ACL_Update_Questions is new Security.Permissions.Definition ("question-update");

   --  ------------------------------
   --  Service services
   --  ------------------------------
   type Question_Service is new AWA.Modules.Module_Manager with private;
   type Question_Service_Access is access all Question_Service'Class;

   --  Create or save the question.
   procedure Save_Question (Model    : in out Question_Service;
                            Question : in out AWA.Questions.Models.Question_Ref'Class);

private

   type Question_Service is new AWA.Modules.Module_Manager with null record;

end AWA.Questions.Services;
