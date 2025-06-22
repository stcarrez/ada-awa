-----------------------------------------------------------------------
--  awa-tags-components -- Tags component
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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

with AWA.Tags.Models;

--  == HTML components ==
--
--  === Displaying a list of tags ===
--  The <tt>awa:tagList</tt> component displays a list of tags.  Each tag can be rendered as
--  a link if the <tt>tagLink</tt> attribute is defined.  The list of tags is passed in the
--  <tt>value</tt> attribute.  When rending that list, the <tt>var</tt> attribute is used to
--  setup a variable with the tag value.  The <tt>tagLink</tt> attribute is then evaluated
--  against that variable and the result defines the link.
--
--    <awa:tagList value='#{questionList.tags}' id='qtags' styleClass="tagedit-list"
--                 tagLink="#{contextPath}/questions/tagged.html?tag=#{tagName}"
--                 var="tagName"
--                 tagClass="tagedit-listelement tagedit-listelement-old"/>
--
--  === Tag editing ===
--  The <tt>awa:tagList</tt> component allows to add or remove tags associated with a given
--  database entity.  The tag management works with the jQuery plugin <b>Tagedit</b>.  For this,
--  the page must include the <b>/js/jquery.tagedit.js</b> Javascript resource.
--
--  The tag edition is active only if the <tt>awa:tagList</tt> component is placed within an
--  <tt>h:form</tt> component.  The <tt>value</tt> attribute defines the list of tags.  This must
--  be a <tt>Tag_List_Bean</tt> object.
--
--    <awa:tagList value='#{question.tags}' id='qtags'
--                 autoCompleteUrl='#{contextPath}/questions/lists/tag-search.html'/>
--
--  When the form is submitted and validated, the procedure <tt>Set_Added</tt> and
--  <tt>Set_Deleted</tt> are called on the value bean with the list of tags that were
--  added and removed.  These operations are called in the <tt>UPDATE_MODEL_VALUES</tt>
--  phase (ie, before calling the action's bean operation).
--
--  === Tag cloud ===
--  The <tt>awa:tagCloud</tt> component displays a list of tags as a tag cloud.
--  The tags list passed in the <tt>value</tt> attribute must inherit from the
--  <tt>Tag_Info_List_Bean</tt> type which indicates for each tag the number of
--  times it is used.
--
--    <awa:tagCloud value='#{questionTagList}' id='cloud' styleClass="tag-cloud"
--                  var="tagName" rows="30"
--                  tagLink="#{contextPath}/questions/tagged.html?tag=#{tagName}"
--                  tagClass="tag-link"/>
--
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

   --  ------------------------------
   --  Tag Cloud Component
   --  ------------------------------
   --  The tag cloud component
   type Tag_UICloud is new ASF.Components.Html.UIHtmlComponent with null record;

   type Tag_Info_Array is array (Positive range <>) of AWA.Tags.Models.Tag_Info;
   type Tag_Info_Array_Access is access all Tag_Info_Array;

   --  Render the tag cloud component.
   overriding
   procedure Encode_Children (UI      : in Tag_UICloud;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Compute the weight for each tag in the list according to the <tt>minWeight</tt> and
   --  <tt>maxWeight</tt> attributes.  The computed weight is an integer multiplied by 100
   --  and will range from 100x<i>minWeight</i> and 100x<i>maxWeight</i>.
   procedure Compute_Cloud_Weight (UI           : in Tag_UICloud;
                                   List         : in out Tag_Info_Array;
                                   Context      : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the list of tags.  If the <tt>tagLink</tt> attribute is defined, a link
   --  is rendered for each tag.
   procedure Render_Cloud (UI      : in Tag_UICloud;
                           List    : in Tag_Info_Array;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end AWA.Tags.Components;
