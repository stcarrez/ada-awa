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

package body Atlas.Reviews.Beans is

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Review_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         return Atlas.Reviews.Models.Review_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Review_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "title" then
         From.Set_Title (Util.Beans.Objects.To_String (Value));
      elsif Name = "site" then
         From.Set_Site (Util.Beans.Objects.To_String (Value));
      elsif Name = "text" then
         From.Set_Text (Util.Beans.Objects.To_String (Value));
      end if;
   end Set_Value;

   overriding
   procedure Save (Bean : in out Review_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Save (Bean);
   end Save;

   overriding
   procedure Delete (Bean : in out Review_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Delete (Bean);
   end Delete;

   --  ------------------------------
   --  Create the Review_Bean bean instance.
   --  ------------------------------
   function Create_Review_Bean (Module : in Atlas.Reviews.Modules.Review_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Review_Bean_Access := new Review_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Review_Bean;

end Atlas.Reviews.Beans;
