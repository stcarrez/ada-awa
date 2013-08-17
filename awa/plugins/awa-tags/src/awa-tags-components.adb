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
with Ada.Strings.Unbounded;
with Ada.Exceptions;

with Util.Beans.Basic;
with Util.Strings;
with Util.Log.Loggers;

with ASF.Utils;
with ASF.Converters;
with ASF.Views.Nodes;
with ASF.Components;
with ASF.Components.Utils;
with ASF.Components.Base;
with ASF.Requests;
with ASF.Events.Faces.Actions;
with ASF.Applications.Main;

with AWA.Tags.Beans;

package body AWA.Tags.Components is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("AWA.Tags.Components");

   READONLY_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   function Create_Tag return ASF.Components.Base.UIComponent_Access;

   --  ------------------------------
   --  Create an Tag_UIInput component
   --  ------------------------------
   function Create_Tag return ASF.Components.Base.UIComponent_Access is
   begin
      return new Tag_UIInput;
   end Create_Tag;

   URI                : aliased constant String := "http://code.google.com/p/ada-awa/jsf";
   TAG_LIST_TAG       : aliased constant String := "tagList";

   AWA_Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => TAG_LIST_TAG'Access,
               Component => Create_Tag'Access,
               Tag       => ASF.Views.Nodes.Create_Component_Node'Access)
        );

   AWA_Factory : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => AWA_Bindings'Access);

   --  ------------------------------
   --  Get the Tags component factory.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return AWA_Factory'Access;
   end Definition;

   --  ------------------------------
   --  Returns True if the tag component must be rendered as readonly.
   --  ------------------------------
   function Is_Readonly (UI      : in Tag_UIInput;
                         Context : in ASF.Contexts.Faces.Faces_Context'Class) return Boolean is
      pragma Unreferenced (Context);
      use type ASF.Components.Html.Forms.UIForm_Access;
   begin
      return UI.Get_Form = null;
   end Is_Readonly;

   --  ------------------------------
   --  Render the javascript to enable the tag edition.
   --  ------------------------------
   procedure Render_Script (UI      : in Tag_UIInput;
                            Id      : in String;
                            Writer  : in Response_Writer_Access;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Auto_Complete : constant String := UI.Get_Attribute ("autoCompleteUrl", Context);
      Allow_Edit    : constant Boolean := UI.Get_Attribute ("allowEdit", Context);
   begin
      Writer.Queue_Script ("$('#");
      Writer.Queue_Script (Id);
      Writer.Queue_Script (" input').tagedit({");
      Writer.Queue_Script ("allowEdit: ");
      if Allow_Edit then
         Writer.Queue_Script ("true");
      else
         Writer.Queue_Script ("false");
      end if;
      if Auto_Complete'Length > 0 then
         Writer.Queue_Script (", autocompleteURL: '");
         Writer.Queue_Script (Auto_Complete);
         Writer.Queue_Script ("'");
      end if;
      Writer.Queue_Script ("});");
   end Render_Script;

   --  ------------------------------
   --  Render the input field title.
   --  ------------------------------
   procedure Render_Tag (UI       : in Tag_UIInput;
                         Id       : in String;
                         Tag      : in Util.Beans.Objects.Object;
                         Class    : in Util.Beans.Objects.Object;
                         Readonly : in Boolean;
                         Writer   : in Response_Writer_Access;
                         Context  : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if Readonly then
         Writer.Start_Element ("li");
         --  Apply the tag class but only on the readonly item.
         if not Util.Beans.Objects.Is_Null (Class) then
            Writer.Write_Attribute ("class", Class);
         end if;
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
         Writer.Write_Attribute (Name => "name", Value => Id);
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
         Writer.End_Element ("input");
      end if;
   end Render_Tag;

   --  ------------------------------
   --  Render the input component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in Tag_UIInput;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         use AWA.Tags.Beans;

         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Value    : constant Util.Beans.Objects.Object := Tag_UIInput'Class (UI).Get_Value;
         Bean     : constant access Util.Beans.Basic.Readonly_Bean'Class
           := Util.Beans.Objects.To_Bean (Value);
         Readonly : constant Boolean := UI.Is_Readonly (Context);
         List     : Tag_List_Bean_Access;
         Count    : Natural;
         Id       : constant String := Ada.Strings.Unbounded.To_String (UI.Get_Client_Id);
         Class    : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "tagClass");
      begin
         if Bean = null then
            ASF.Components.Base.Log_Error (UI, "There is no tagList bean value");
            return;
         elsif not (Bean.all in Tag_List_Bean'Class) then
            ASF.Components.Base.Log_Error (UI, "There is not taglist bean");
            return;
         end if;

         List := Tag_List_Bean'Class (Bean.all)'Unchecked_Access;
         Count := List.Get_Count;
         if Count > 0 then
            if Readonly then
               Writer.Start_Element ("ul");
            else
               Writer.Start_Element ("div");
            end if;
            UI.Render_Attributes (Context, Writer);
            for I in 1 .. Count loop
               List.Set_Row_Index (I);
               Tag_UIInput'Class (UI).Render_Tag (Id & "[" & Util.Strings.Image (I) & "-a]",
                                                  List.Get_Row, Class, Readonly, Writer, Context);
            end loop;
            if Readonly then
               Writer.End_Element ("ul");
            end if;
         elsif not Readonly then
            Writer.Start_Element ("div");
            Writer.Write_Attribute ("id", Id);
         end if;
         if not Readonly then
            Tag_UIInput'Class (UI).Render_Tag (Id & "[]", Util.Beans.Objects.Null_Object,
                                               Util.Beans.Objects.Null_Object, Readonly,
                                               Writer, Context);
            Writer.End_Element ("div");
            Tag_UIInput'Class (UI).Render_Script (Id, Writer, Context);
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

   --  ------------------------------
   --  Get the action method expression to invoke if the command is pressed.
   --  ------------------------------
   function Get_Action_Expression (UI      : in Tag_UIInput;
                                   Context : in ASF.Contexts.Faces.Faces_Context'Class)
                                   return EL.Expressions.Method_Expression is
      pragma Unreferenced (Context);
   begin
      return UI.Get_Method_Expression (Name => ASF.Components.ACTION_NAME);
   end Get_Action_Expression;

   --  ------------------------------
   --  Decode any new state of the specified component from the request contained
   --  in the specified context and store that state on the component.
   --
   --  During decoding, events may be enqueued for later processing
   --  (by event listeners that have registered an interest), by calling
   --  the <b>Queue_Event</b> on the associated component.
   --  ------------------------------
   overriding
   procedure Process_Decodes (UI      : in out Tag_UIInput;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Req : constant ASF.Requests.Request_Access := Context.Get_Request;
         Id  : constant String := Ada.Strings.Unbounded.To_String (UI.Get_Client_Id);

         procedure Process_Parameter (Name, Value : in String);

         procedure Process_Parameter (Name, Value : in String) is
         begin
            if Name'Length <= Id'Length or Name (Name'Last) /= ']' then
               return;
            end if;
            if Name (Name'First .. Name'First + Id'Length - 1) /= Id then
               return;
            end if;
            if Name (Name'First + Id'Length) /= '[' then
               return;
            end if;
            if Name (Name'Last - 1) = 'd' then
               UI.Deleted.Append (Value);
            elsif Name (Name'Last - 1) /= 'a' then
               UI.Added.Append (Value);
            end if;
         end Process_Parameter;

         Val : constant String := Context.Get_Parameter (Id);
      begin
         Req.Iterate_Parameters (Process_Parameter'Access);
         UI.Is_Valid := True;
         if not UI.Added.Is_Empty or else not UI.Deleted.Is_Empty then
            ASF.Events.Faces.Actions.Post_Event (UI     => UI,
                                                 Method => UI.Get_Action_Expression (Context));
         end if;

      exception
         when E : others =>
            UI.Is_Valid := False;
            Log.Info (ASF.Components.Utils.Get_Line_Info (UI)
                      & ": Exception raised when converting value {0} for component {1}: {2}",
                      Val, Id, Ada.Exceptions.Exception_Name (E));
      end;
   end Process_Decodes;

   --  ------------------------------
   --  Perform the component tree processing required by the <b>Update Model Values</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows.
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Updates/b> of all facets and children.
   --  <ul>
   --  ------------------------------
   overriding
   procedure Process_Updates (UI      : in out Tag_UIInput;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if UI.Is_Valid and then not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         use AWA.Tags.Beans;

         Value    : constant Util.Beans.Objects.Object := Tag_UIInput'Class (UI).Get_Value;
         Bean     : constant access Util.Beans.Basic.Readonly_Bean'Class
           := Util.Beans.Objects.To_Bean (Value);
         List     : access Tag_List_Bean'Class;
      begin
         if Bean = null then
            ASF.Components.Base.Log_Error (UI, "There is no tagList bean value");
            return;
         elsif not (Bean.all in Tag_List_Bean'Class) then
            ASF.Components.Base.Log_Error (UI, "There is not taglist bean");
            return;
         end if;
         List := Tag_List_Bean'Class (Bean.all)'Access;
         List.Set_Added (UI.Added);
         List.Set_Deleted (UI.Deleted);
      end;
   end Process_Updates;

   --  ------------------------------
   --  Broadcast the event to the event listeners installed on this component.
   --  Listeners are called in the order in which they were added.
   --  ------------------------------
   overriding
   procedure Broadcast (UI      : in out Tag_UIInput;
                        Event   : not null access ASF.Events.Faces.Faces_Event'Class;
                        Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (UI);

      use ASF.Events.Faces.Actions;

      App  : constant access ASF.Applications.Main.Application'Class := Context.Get_Application;
      Disp : constant Action_Listener_Access := App.Get_Action_Listener;
   begin
      if Disp /= null and Event.all in Action_Event'Class then
         Disp.Process_Action (Event   => Action_Event (Event.all),
                              Context => Context);
      end if;
   end Broadcast;

begin
   ASF.Utils.Set_Text_Attributes (READONLY_ATTRIBUTE_NAMES);
end AWA.Tags.Components;
