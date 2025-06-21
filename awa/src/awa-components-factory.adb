-----------------------------------------------------------------------
--  awa-components-factory -- Factory for AWA UI Components
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Components.Base;
with AWA.Components.Redirect;
with AWA.Components.Inputs;
with AWA.Components.Wikis;
with ASF.Views.Nodes;
package body AWA.Components.Factory is

   use ASF.Components.Base;

   function Create_Redirect return UIComponent_Access;
   function Create_Input return UIComponent_Access;
   function Create_Wiki return UIComponent_Access;

   --  ------------------------------
   --  Create an UIRedirect component
   --  ------------------------------
   function Create_Redirect return UIComponent_Access is
   begin
      return new AWA.Components.Redirect.UIRedirect;
   end Create_Redirect;

   --  ------------------------------
   --  Create an UIInput component
   --  ------------------------------
   function Create_Input return UIComponent_Access is
   begin
      return new AWA.Components.Inputs.UIInput;
   end Create_Input;

   --  ------------------------------
   --  Create an UIWiki component
   --  ------------------------------
   function Create_Wiki return UIComponent_Access is
   begin
      return new AWA.Components.Wikis.UIWiki;
   end Create_Wiki;

   use ASF.Views.Nodes;

   URI                : aliased constant String := "http://code.google.com/p/ada-awa/jsf";
   REDIRECT_TAG       : aliased constant String := "redirect";
   INPUT_TEXT_TAG     : aliased constant String := "inputText";
   WIKI_TAG           : aliased constant String := "wiki";

   AWA_Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => INPUT_TEXT_TAG'Access,
               Component => Create_Input'Access,
               Tag       => Create_Component_Node'Access),
         2 => (Name      => REDIRECT_TAG'Access,
               Component => Create_Redirect'Access,
               Tag       => Create_Component_Node'Access),
         3 => (Name      => WIKI_TAG'Access,
               Component => Create_Wiki'Access,
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
