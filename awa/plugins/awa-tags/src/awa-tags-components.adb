-----------------------------------------------------------------------
--  awa-tags-components -- Tags component
--  Copyright (C) 2013, 2014, 2015, 2018, 2019, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

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
   function Create_Cloud return ASF.Components.Base.UIComponent_Access;
   procedure Swap (Item1 : in out AWA.Tags.Models.Tag_Info;
                   Item2 : in out AWA.Tags.Models.Tag_Info);
   procedure Dispatch (List : in out Tag_Info_Array);

   --  ------------------------------
   --  Create an Tag_UIInput component
   --  ------------------------------
   function Create_Tag return ASF.Components.Base.UIComponent_Access is
   begin
      return new Tag_UIInput;
   end Create_Tag;

   --  ------------------------------
   --  Create an Tag_UICloud component
   --  ------------------------------
   function Create_Cloud return ASF.Components.Base.UIComponent_Access is
   begin
      return new Tag_UICloud;
   end Create_Cloud;

   URI                : aliased constant String := "http://code.google.com/p/ada-awa/jsf";
   TAG_LIST_TAG       : aliased constant String := "tagList";
   TAG_CLOUD_TAG      : aliased constant String := "tagCloud";

   AWA_Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => TAG_CLOUD_TAG'Access,
               Component => Create_Cloud'Access,
               Tag       => ASF.Views.Nodes.Create_Component_Node'Access),
         2 => (Name      => TAG_LIST_TAG'Access,
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
   --  Get the tag after convertion with the optional converter.
   --  ------------------------------
   function Get_Tag (UI      : in Tag_UIInput;
                     Tag     : in Util.Beans.Objects.Object;
                     Context : in ASF.Contexts.Faces.Faces_Context'Class) return String is
   begin
      if not Util.Beans.Objects.Is_Null (Tag) then
         declare
            Convert : constant access ASF.Converters.Converter'Class
              := Tag_UIInput'Class (UI).Get_Converter;
         begin
            if Convert /= null then
               return Convert.To_String (Value     => Tag,
                                         Component => UI,
                                         Context   => Context);
            else
               return Util.Beans.Objects.To_String (Value => Tag);
            end if;
         end;
      else
         return "";
      end if;
   end Get_Tag;

   --  ------------------------------
   --  Render the tag as a readonly item.
   --  ------------------------------
   procedure Render_Readonly_Tag (UI       : in Tag_UIInput;
                                  Tag      : in Util.Beans.Objects.Object;
                                  Class    : in Util.Beans.Objects.Object;
                                  Writer   : in Response_Writer_Access;
                                  Context  : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Value : constant String := UI.Get_Tag (Tag, Context);
   begin
      Writer.Start_Element ("li");
      if not Util.Beans.Objects.Is_Null (Class) then
         Writer.Write_Attribute ("class", Class);
      end if;
      Writer.Start_Element ("span");
      Writer.Write_Text (Value);
      Writer.End_Element ("span");
      Writer.End_Element ("li");
   end Render_Readonly_Tag;

   --  ------------------------------
   --  Render the tag as a link.
   --  ------------------------------
   procedure Render_Link_Tag (UI       : in Tag_UIInput;
                              Name     : in String;
                              Tag      : in Util.Beans.Objects.Object;
                              Link     : in EL.Expressions.Expression;
                              Class    : in Util.Beans.Objects.Object;
                              Writer   : in Response_Writer_Access;
                              Context  : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Value : constant String := UI.Get_Tag (Tag, Context);
   begin
      Context.Set_Attribute (Name, Util.Beans.Objects.To_Object (Value));
      Writer.Start_Element ("li");
      if not Util.Beans.Objects.Is_Null (Class) then
         Writer.Write_Attribute ("class", Class);
      end if;
      Writer.Start_Element ("a");
      Writer.Write_Attribute ("href", Link.Get_Value (Context.Get_ELContext.all));
      Writer.Write_Text (Value);
      Writer.End_Element ("a");
      Writer.End_Element ("li");
   end Render_Link_Tag;

   --  ------------------------------
   --  Render the tag for an input form.
   --  ------------------------------
   procedure Render_Form_Tag (UI       : in Tag_UIInput;
                              Id       : in String;
                              Tag      : in Util.Beans.Objects.Object;
                              Writer   : in Response_Writer_Access;
                              Context  : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
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
   end Render_Form_Tag;

   --  ------------------------------
   --  Render the list of tags as readonly list.  If a <tt>tagLink</tt> attribute is defined,
   --  a link is rendered for each tag.  Otherwise, each tag is rendered as a <tt>span</tt>.
   --  ------------------------------
   procedure Render_Readonly (UI      : in Tag_UIInput;
                              List    : in Util.Beans.Basic.List_Bean_Access;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Class  : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "tagClass");
      Count  : constant Natural := List.Get_Count;
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
      Link   : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute ("tagLink");
   begin
      if Count > 0 then
         Writer.Start_Element ("ul");
         UI.Render_Attributes (Context, Writer);
         if Link /= null then
            declare
               Link : constant EL.Expressions.Expression := UI.Get_Expression ("tagLink");
               Name : constant String := UI.Get_Attribute (Name    => "var",
                                                           Context => Context,
                                                           Default => "tag");
            begin
               for I in 1 .. Count loop
                  List.Set_Row_Index (I);
                  Tag_UIInput'Class (UI).Render_Link_Tag (Name    => Name,
                                                          Tag     => List.Get_Row,
                                                          Link    => Link,
                                                          Class   => Class,
                                                          Writer  => Writer,
                                                          Context => Context);
               end loop;
            end;
         else
            for I in 1 .. Count loop
               List.Set_Row_Index (I);
               Tag_UIInput'Class (UI).Render_Readonly_Tag (Tag     => List.Get_Row,
                                                           Class   => Class,
                                                           Writer  => Writer,
                                                           Context => Context);
            end loop;
         end if;
         Writer.End_Element ("ul");
      end if;
   end Render_Readonly;

   --  ------------------------------
   --  Render the list of tags for a form.
   --  ------------------------------
   procedure Render_Form (UI      : in Tag_UIInput;
                          List    : in Util.Beans.Basic.List_Bean_Access;
                          Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Id     : constant String := Ada.Strings.Unbounded.To_String (UI.Get_Client_Id);
      Count  : constant Natural := List.Get_Count;
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      Writer.Start_Element ("div");
      UI.Render_Attributes (Context, Writer);
      if Count > 0 then
         for I in 1 .. Count loop
            List.Set_Row_Index (I);
            Tag_UIInput'Class (UI).Render_Form_Tag (Id & "[" & Util.Strings.Image (I) & "]",
                                                    List.Get_Row, Writer, Context);
         end loop;
      else
         Writer.Write_Attribute ("id", Id);
      end if;
      Tag_UIInput'Class (UI).Render_Form_Tag (Id & "[]", Util.Beans.Objects.Null_Object,
                                              Writer, Context);
      Writer.End_Element ("div");
      Tag_UIInput'Class (UI).Render_Script (Id, Writer, Context);
   end Render_Form;

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
         Value    : constant Util.Beans.Objects.Object := Tag_UIInput'Class (UI).Get_Value;
         Bean     : constant access Util.Beans.Basic.Readonly_Bean'Class
           := Util.Beans.Objects.To_Bean (Value);
         Readonly : constant Boolean := UI.Is_Readonly (Context);
         List     : Util.Beans.Basic.List_Bean_Access;
      begin
         if Bean = null then
            if not Readonly then
               ASF.Components.Base.Log_Error (UI, "There is no tagList bean value");
            end if;
            return;
         elsif not (Bean.all in Util.Beans.Basic.List_Bean'Class) then
            ASF.Components.Base.Log_Error (UI, "There bean value is not a List_Bean");
            return;
         end if;

         List := Util.Beans.Basic.List_Bean'Class (Bean.all)'Unchecked_Access;
         if Readonly then
            UI.Render_Readonly (List, Context);
         else
            UI.Render_Form (List, Context);
         end if;
      end;
   end Encode_Begin;

   --  ------------------------------
   --  Render the end of the input component.  Closes the DL/DD list.
   --  ------------------------------
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
            if Name'Length <= Id'Length or else Name (Name'Last) /= ']' then
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
            elsif Name (Name'Last - 1) = 'a' or else Name (Name'Last - 1) = '[' then
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
      if Disp /= null and then Event.all in Action_Event'Class then
         Disp.Process_Action (Event   => Action_Event (Event.all),
                              Context => Context);
      end if;
   end Broadcast;

   --  ------------------------------
   --  Render the list of tags.  If the <tt>tagLink</tt> attribute is defined, a link
   --  is rendered for each tag.
   --  ------------------------------
   procedure Render_Cloud (UI      : in Tag_UICloud;
                           List    : in Tag_Info_Array;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
      Link   : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute ("tagLink");
      Style  : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "tagClass");
      Font_Size : constant Boolean := True;
   begin
      Writer.Start_Element ("ul");
      UI.Render_Attributes (Context, Writer);
      if Link /= null then
         declare
            Link : constant EL.Expressions.Expression := UI.Get_Expression ("tagLink");
            Name : constant String := UI.Get_Attribute ("var", Context, "");
         begin
            for I in List'Range loop
               Writer.Start_Element ("li");
               if not Util.Beans.Objects.Is_Null (Style) then
                  Writer.Write_Attribute ("class", Style);
               end if;
               Writer.Start_Element ("a");
               Context.Set_Attribute (Name, Util.Beans.Objects.To_Object (List (I).Tag));
               Writer.Write_Attribute ("href", Link.Get_Value (Context.Get_ELContext.all));
               if Font_Size then
                  Writer.Write_Attribute ("style",
                                          "font-size:" & Util.Strings.Image (List (I).Count / 100)
                                          & "." & Util.Strings.Image (List (I).Count mod 100)
                                          & "px;");
               else
                  Writer.Write_Attribute ("class", "tag"
                                          & Util.Strings.Image (List (I).Count / 100));
               end if;
               Writer.Write_Text (List (I).Tag);
               Writer.End_Element ("a");
               Writer.End_Element ("li");
            end loop;
         end;
      else
         for I in List'Range loop
            Writer.Start_Element ("li");
            if not Util.Beans.Objects.Is_Null (Style) then
               Writer.Write_Attribute ("class", Style);
            end if;
            Writer.Write_Text (List (I).Tag);
            Writer.End_Element ("li");
         end loop;
      end if;
      Writer.End_Element ("ul");
   end Render_Cloud;

   --  ------------------------------
   --  Compute the weight for each tag in the list according to the <tt>minWeight</tt> and
   --  <tt>maxWeight</tt> attributes.  The computed weight is an integer multiplied by 100
   --  and will range from 100x<i>minWeight</i> and 100x<i>maxWeight</i>.
   --  ------------------------------
   procedure Compute_Cloud_Weight (UI           : in Tag_UICloud;
                                   List         : in out Tag_Info_Array;
                                   Context      : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Min       : constant Integer := UI.Get_Attribute ("minWeight", Context, 0);
      Max       : constant Integer := UI.Get_Attribute ("maxWeight", Context, 100) - Min;
      Min_Count : constant Natural := List (List'Last).Count;
      Max_Count : Natural := List (List'First).Count;
   begin
      if Max_Count = Min_Count then
         Max_Count := Min_Count + 1;
      end if;
      for I in List'Range loop
         if List (I).Count = Min_Count then
            List (I).Count := 100 * Min;
         else
            List (I).Count := (100 * Max * (List (I).Count - Min_Count)) / (Max_Count - Min_Count)
              + 100 * Min;
         end if;
      end loop;
   end Compute_Cloud_Weight;

   procedure Swap (Item1 : in out AWA.Tags.Models.Tag_Info;
                   Item2 : in out AWA.Tags.Models.Tag_Info) is
      Tmp : constant AWA.Tags.Models.Tag_Info := Item2;
   begin
      Item2 := Item1;
      Item1 := Tmp;
   end Swap;

   --  ------------------------------
   --  A basic algorithm to dispatch the tags in the list.
   --  The original list is sorted on the tag count (due to the Tag_Ordered_Sets).
   --  We try to dispatch the first tags somewhere in the first or second halves of the list.
   --  ------------------------------
   procedure Dispatch (List : in out Tag_Info_Array) is
      Middle  : constant Natural := List'First + List'Length / 2;
      Quarter : Natural := List'Length / 4;
      Target  : Natural := Middle;
      Pos     : Natural := 0;
   begin
      while Target <= List'Last and then List'First + Pos < Middle and then Quarter /= 0 loop
         Swap (List (List'First + Pos), List (Target));
         Pos := Pos + 1;
         if Target <= Middle then
            Target := List'Last - Quarter;
         else
            Target := List'First + Quarter;
            Quarter := Quarter / 2;
         end if;
      end loop;
   end Dispatch;

   --  ------------------------------
   --  Render the tag cloud component.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in Tag_UICloud;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is

      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Tag_Info_Array,
                                         Name   => Tag_Info_Array_Access);

      Table : Tag_Info_Array_Access := null;
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      declare
         use type Util.Beans.Basic.List_Bean_Access;

         Max   : constant Integer := UI.Get_Attribute ("rows", Context, 1000);
         Layout : constant String := UI.Get_Attribute ("layout", Context);
         Bean  : constant Util.Beans.Basic.List_Bean_Access
           := ASF.Components.Utils.Get_List_Bean (UI, "value", Context);
         Count : Natural;
         Tags  : AWA.Tags.Beans.Tag_Ordered_Sets.Set;
         List  : AWA.Tags.Beans.Tag_Info_List_Bean_Access;
      begin
         --  Check that we have a List_Bean but do not complain if we have a null value.
         if Bean = null or else Max <= 0 then
            return;
         end if;

         if not (Bean.all in AWA.Tags.Beans.Tag_Info_List_Bean'Class) then
            ASF.Components.Base.Log_Error (UI, "Invalid tag list bean: it does not "
                                           & "implement 'Tag_Info_List_Bean' interface");
            return;
         end if;

         List := AWA.Tags.Beans.Tag_Info_List_Bean'Class (Bean.all)'Unchecked_Access;
         Count := List.Get_Count;
         if Count = 0 then
            return;
         end if;

         --  Pass 1: Collect the tags and keep the most used.
         for I in 1 .. Count loop
            Tags.Insert (List.List.Element (I));
            if Integer (Tags.Length) > Max then
               Tags.Delete_Last;
            end if;
         end loop;

         Count := Natural (Tags.Length);
         Table := new Tag_Info_Array (1 .. Count);
         for I in 1 .. Count loop
            Table (I) := Tags.First_Element;
            Tags.Delete_First;
         end loop;

         --  Pass 2: Assign weight to each tag.
         UI.Compute_Cloud_Weight (Table.all, Context);

         --  Pass 3: Dispatch the tags using some layout algorithm.
         if Layout = "dispatch" then
            Dispatch (Table.all);
         end if;

         --  Pass 4: Render each tag.
         UI.Render_Cloud (Table.all, Context);

         Free (Table);

      exception
         when others =>
            Free (Table);
            raise;
      end;
   end Encode_Children;

begin
   ASF.Utils.Set_Text_Attributes (READONLY_ATTRIBUTE_NAMES);
end AWA.Tags.Components;
