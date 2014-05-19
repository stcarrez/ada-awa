-----------------------------------------------------------------------
--  atlas-reviews-beans -- Beans for module reviews
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

with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Methods;
with Atlas.Reviews.Modules;
with Atlas.Reviews.Models;
package Atlas.Reviews.Beans is

   type Review_Bean is new Atlas.Reviews.Models.Review_Bean with record
      Module : Atlas.Reviews.Modules.Review_Module_Access := null;
   end record;
   type Review_Bean_Access is access all Review_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Review_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Review_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   overriding
   procedure Save (Bean : in out Review_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   overriding
   procedure Delete (Bean : in out Review_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Reviews_Bean bean instance.
   function Create_Review_Bean (Module : in Atlas.Reviews.Modules.Review_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

end Atlas.Reviews.Beans;
