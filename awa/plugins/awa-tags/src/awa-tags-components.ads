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

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Strings.Vectors;

with EL.Expressions;

with ASF.Components.Html.Forms;
with ASF.Contexts.Faces;
with ASF.Contexts.Writer;
with ASF.Events.Faces;
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
   type Tag_UIInput is new ASF.Components.Html.Forms.UIInput with record
      --  List of tags that have been added.
      Added    : Util.Strings.Vectors.Vector;

      --  List of tags that have been removed.
      Deleted  : Util.Strings.Vectors.Vector;

      --  True if the submitted values are correct.
      Is_Valid : Boolean := False;
   end record;
   type Tag_UIInput_Access is access all Tag_UIInput'Class;

   --  Returns True if the tag component must be rendered as readonly.
   function Is_Readonly (UI      : in Tag_UIInput;
                         Context : in ASF.Contexts.Faces.Faces_Context'Class) return Boolean;

   --  Get the tag after convertion with the optional converter.
   function Get_Tag (UI      : in Tag_UIInput;
                     Tag     : in Util.Beans.Objects.Object;
                     Context : in ASF.Contexts.Faces.Faces_Context'Class) return String;

   --  Render the tag as a readonly item.
   procedure Render_Readonly_Tag (UI       : in Tag_UIInput;
                                  Tag      : in Util.Beans.Objects.Object;
                                  Class    : in Util.Beans.Objects.Object;
                                  Writer   : in Response_Writer_Access;
                                  Context  : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the tag as a link.
   procedure Render_Link_Tag (UI       : in Tag_UIInput;
                              Name     : in String;
                              Tag      : in Util.Beans.Objects.Object;
                              Link     : in EL.Expressions.Expression;
                              Class    : in Util.Beans.Objects.Object;
                              Writer   : in Response_Writer_Access;
                              Context  : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the tag for an input form.
   procedure Render_Form_Tag (UI       : in Tag_UIInput;
                              Id       : in String;
                              Tag      : in Util.Beans.Objects.Object;
                              Writer   : in Response_Writer_Access;
                              Context  : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  a link is rendered for each tag.  Otherwise, each tag is rendered as a <tt>span</tt>.
   procedure Render_Readonly (UI      : in Tag_UIInput;
                              List    : in Util.Beans.Basic.List_Bean_Access;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the list of tags for a form.
   procedure Render_Form (UI      : in Tag_UIInput;
                          List    : in Util.Beans.Basic.List_Bean_Access;
                          Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the javascript to enable the tag edition.
   procedure Render_Script (UI      : in Tag_UIInput;
                            Id      : in String;
                            Writer  : in Response_Writer_Access;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the input component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   overriding
   procedure Encode_Begin (UI      : in Tag_UIInput;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the end of the input component.  Closes the DL/DD list.
   overriding
   procedure Encode_End (UI      : in Tag_UIInput;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Get the action method expression to invoke if the command is pressed.
   --  ------------------------------
   function Get_Action_Expression (UI      : in Tag_UIInput;
                                   Context : in ASF.Contexts.Faces.Faces_Context'Class)
                                   return EL.Expressions.Method_Expression;

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

   --  Broadcast the event to the event listeners installed on this component.
   --  Listeners are called in the order in which they were added.
   overriding
   procedure Broadcast (UI      : in out Tag_UIInput;
                        Event   : not null access ASF.Events.Faces.Faces_Event'Class;
                        Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end AWA.Tags.Components;
