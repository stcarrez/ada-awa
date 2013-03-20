-----------------------------------------------------------------------
--  awa-tags-components -- Tags component
--  Copyright (C) 2013 Stephane Carrez
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

with Util.Beans.Objects;

with ASF.Components.Html.Forms;
with ASF.Contexts.Faces;
with ASF.Contexts.Writer;
with ASF.Factory;

package AWA.Tags.Components is

   use ASF.Contexts.Writer;

   --  Get the Tags component factory.
   function Definition return ASF.Factory.Factory_Bindings_Access;

   --  ------------------------------
   --  Input component
   --  ------------------------------
   --  The AWA input component overrides the ASF input component to build a compact component
   --  that displays a label, the input field and the associated error message if necessary.
   --
   --  The generated HTML looks like:
   --
   --    <ul class='taglist'>
   --      <li><span>tag</span></li>
   --    </ul>
   --
   --  or
   --
   --    <input type='text' name='' value='tag'/>
   --
   type Tag_UIInput is new ASF.Components.Html.Forms.UIInput with null record;
   type Tag_UIInput_Access is access all Tag_UIInput'Class;

   --  Returns True if the tag component must be rendered as readonly.
   function Is_Readonly (UI      : in Tag_UIInput;
                         Context : in ASF.Contexts.Faces.Faces_Context'Class) return Boolean;

   procedure Render_Tag (UI       : in Tag_UIInput;
                         Tag      : in Util.Beans.Objects.Object;
                         Readonly : in Boolean;
                         Writer   : in Response_Writer_Access;
                         Context  : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the input component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   overriding
   procedure Encode_Begin (UI      : in Tag_UIInput;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the end of the input component.  Closes the DL/DD list.
   overriding
   procedure Encode_End (UI      : in Tag_UIInput;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Decode any new state of the specified component from the request contained
   --  in the specified context and store that state on the component.
   --
   --  During decoding, events may be enqueued for later processing
   --  (by event listeners that have registered an interest), by calling
   --  the <b>Queue_Event</b> on the associated component.
   overriding
   procedure Process_Decodes (UI      : in out Tag_UIInput;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Perform the component tree processing required by the <b>Update Model Values</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows.
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Updates/b> of all facets and children.
   --  <ul>
   overriding
   procedure Process_Updates (UI      : in out Tag_UIInput;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end AWA.Tags.Components;
