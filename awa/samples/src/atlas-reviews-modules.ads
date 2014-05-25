-----------------------------------------------------------------------
--  atlas-reviews-modules -- Module reviews
--  Copyright (C) 2014 Stephane.Carrez
--  Written by Stephane.Carrez (Stephane.Carrez@gmail.com)
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

with AWA.Modules;
with Atlas.Reviews.Models;
with Security.Permissions;
package Atlas.Reviews.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "reviews";

   package ACL_Create_Reviews is new Security.Permissions.Definition ("review-create");
   package ACL_Delete_Reviews is new Security.Permissions.Definition ("review-delete");
   package ACL_Update_Reviews is new Security.Permissions.Definition ("review-update");

   --  ------------------------------
   --  Module reviews
   --  ------------------------------
   type Review_Module is new AWA.Modules.Module with private;
   type Review_Module_Access is access all Review_Module'Class;

   --  Initialize the reviews module.
   overriding
   procedure Initialize (Plugin : in out Review_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the reviews module.
   function Get_Review_Module return Review_Module_Access;


   --  Save the review.
   procedure Save (Model  : in Review_Module;
                   Entity : in out Atlas.Reviews.Models.Review_Ref'Class);

   --  Delete the review.
   procedure Delete (Model  : in Review_Module;
                     Entity : in out Atlas.Reviews.Models.Review_Ref'Class);
private

   type Review_Module is new AWA.Modules.Module with null record;

end Atlas.Reviews.Modules;
