-----------------------------------------------------------------------
--  awa-components-factory -- Factory for AWA UI Components
--  Copyright (C) 2011 Stephane Carrez
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

with ASF.Components.Base;
with AWA.Components.Redirect;
with ASF.Views.Nodes;
package body AWA.Components.Factory is

   use ASF.Components.Base;

   function Create_Redirect return UIComponent_Access;

   --  ------------------------------
   --  Create an UIRedirect component
   --  ------------------------------
   function Create_Redirect return UIComponent_Access is
   begin
      return new AWA.Components.Redirect.UIRedirect;
   end Create_Redirect;

   use ASF.Views.Nodes;

   URI                : aliased constant String := "http://code.google.com/p/ada-awa/jsf";
   REDIRECT_TAG       : aliased constant String := "redirect";

   AWA_Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => REDIRECT_TAG'Access,
               Component => Create_Redirect'Access,
               Tag       => Create_Component_Node'Access)
        );

   AWA_Factory : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => AWA_Bindings'Access);

   --  ------------------------------
   --  Get the AWA component factory.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return AWA_Factory'Access;
   end Definition;

end AWA.Components.Factory;
