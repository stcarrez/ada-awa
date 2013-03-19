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
with Util.Strings;

with ASF.Utils;
with ASF.Contexts.Writer;
with ASF.Converters;

with AWA.Tags.Beans;

package body AWA.Tags.Components is

   use ASF.Contexts.Faces;
   use ASF.Contexts.Writer;

   INPUT_ATTRIBUTE_NAMES     : Util.Strings.String_Set.Set;
   READONLY_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  Returns True if the tag component must be rendered as readonly.
   --  ------------------------------
   function Is_Readonly (UI      : in Tag_UIInput;
                         Context : in ASF.Contexts.Faces.Faces_Context'Class) return Boolean is
      use type ASF.Components.Html.Forms.UIForm_Access;
   begin
      return UI.Get_Form = null;
   end Is_Readonly;

   --  Render the input field title.
   procedure Render_Tag (UI       : in Tag_UIInput;
                         Tag      : in Util.Beans.Objects.Object;
                         Readonly : in Boolean;
                         Writer   : in Response_Writer_Access;
                         Context  : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if Readonly then
         Writer.Start_Element ("li");
         UI.Render_Attributes (Context, READONLY_ATTRIBUTE_NAMES, Writer);
         Writer.Start_Element ("span");
         if not Util.Beans.Objects.Is_Null (Tag) then
            declare
               Convert : constant access ASF.Converters.Converter'Class
                 := Tag_UIInput'Class (UI).Get_Converter;
            begin
               if Convert /= null then
                  Writer.Write_Text (Text => Convert.To_String (Value     => Tag,
                                                                Component => UI,
                                                                Context   => Context));
               else
                  Writer.Write_Text (Value => Tag);
               end if;
            end;
         end if;
         Writer.End_Element ("span");
         Writer.End_Element ("li");
      else
         Writer.Start_Element ("input");
         Writer.Write_Attribute (Name => "type", Value => "text");
         Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
         if not Util.Beans.Objects.Is_Null (Tag) then
            declare
               Convert : constant access ASF.Converters.Converter'Class
                 := Tag_UIInput'Class (UI).Get_Converter;
            begin
               if Convert /= null then
                  Writer.Write_Attribute (Name  => "value",
                                          Value => Convert.To_String (Value     => Tag,
                                                                      Component => UI,
                                                                      Context   => Context));
               else
                  Writer.Write_Attribute (Name => "value", Value => Tag);
               end if;
            end;
         end if;
         UI.Render_Attributes (Context, INPUT_ATTRIBUTE_NAMES, Writer);
         Writer.End_Element ("input");
      end if;
   end Render_Tag;

   --  Render the input component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   overriding
   procedure Encode_Begin (UI      : in Tag_UIInput;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         use AWA.Tags.Beans;

         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Value    : constant Util.Beans.Objects.Object := Tag_UIInput'Class (UI).Get_Value;
         Bean     : constant access Util.Beans.Basic.Readonly_Bean'Class := Util.Beans.Objects.To_Bean (Value);
         Readonly : constant Boolean := UI.Is_Readonly (Context);
         List     : Tag_List_Bean_Access;
         Count    : Natural;
      begin
         if Bean /= null and then Bean.all in Tag_List_Bean'Class then
            List := Tag_List_Bean'Class (Bean.all)'Unchecked_Access;
            Count := List.Get_Count;
            if Count > 0 then
               if Readonly then
                  Writer.Start_Element ("ul");
               end if;
               for I in 1 .. Count loop
                  List.Set_Row_Index (I);
                  Tag_UIInput'Class (UI).Render_Tag (List.Get_Row, Readonly, Writer, Context);
               end loop;
               if Readonly then
                  Writer.End_Element ("ul");
               end if;
            end if;
            if not Readonly then
               Tag_UIInput'Class (UI).Render_Tag (Util.Beans.Objects.Null_Object, Readonly,
                                                  Writer, Context);
            end if;
         end if;
      end;
   end Encode_Begin;

   --  Render the end of the input component.  Closes the DL/DD list.
   overriding
   procedure Encode_End (UI      : in Tag_UIInput;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      null;
   end Encode_End;

   --  Decode any new state of the specified component from the request contained
   --  in the specified context and store that state on the component.
   --
   --  During decoding, events may be enqueued for later processing
   --  (by event listeners that have registered an interest), by calling
   --  the <b>Queue_Event</b> on the associated component.
   overriding
   procedure Process_Decodes (UI      : in out Tag_UIInput;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      null;
   end Process_Decodes;

   --  Perform the component tree processing required by the <b>Update Model Values</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows.
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Updates/b> of all facets and children.
   --  <ul>
   overriding
   procedure Process_Updates (UI      : in out Tag_UIInput;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      null;
   end Process_Updates;

begin
   ASF.Utils.Set_Text_Attributes (INPUT_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Text_Attributes (READONLY_ATTRIBUTE_NAMES);
end AWA.Tags.Components;
