-----------------------------------------------------------------------
--  awa-counters-components -- Counter UI component
--  Copyright (C) 2015, 2022 Stephane Carrez
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
with Util.Beans.Basic;
with AWA.Counters.Beans;
with ASF.Components.Base;
with ASF.Views.Nodes;
with ASF.Utils;
with ASF.Contexts.Writer;
package body AWA.Counters.Components is

   TEXT_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   function Create_Counter return ASF.Components.Base.UIComponent_Access;

   --  ------------------------------
   --  Create a UICounter component.
   --  ------------------------------
   function Create_Counter return ASF.Components.Base.UIComponent_Access is
   begin
      return new UICounter;
   end Create_Counter;

   URI                : aliased constant String := "http://code.google.com/p/ada-awa/jsf";
   COUNTER_TAG        : aliased constant String := "counter";

   AWA_Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => COUNTER_TAG'Access,
               Component => Create_Counter'Access,
               Tag       => ASF.Views.Nodes.Create_Component_Node'Access)
        );

   AWA_Factory : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => AWA_Bindings'Access);

   --  ------------------------------
   --  Get the AWA Counter component factory.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return AWA_Factory'Access;
   end Definition;

   --  ------------------------------
   --  Check if the counter value is hidden.
   --  ------------------------------
   function Is_Hidden (UI      : in UICounter;
                       Context : in ASF.Contexts.Faces.Faces_Context'Class) return Boolean is
      Value : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "hidden");
   begin
      return Util.Beans.Objects.To_Boolean (Value);
   end Is_Hidden;

   --  ------------------------------
   --  Render the counter component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UICounter;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Value    : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "value");
         Bean     : constant access Util.Beans.Basic.Readonly_Bean'Class
           := Util.Beans.Objects.To_Bean (Value);
         Writer   : constant ASF.Contexts.Writer.Response_Writer_Access
           := Context.Get_Response_Writer;
         Counter  : AWA.Counters.Beans.Counter_Bean_Access;
      begin
         if Bean = null then
            ASF.Components.Base.Log_Error (UI, "There is no counter bean value");
            return;
         elsif not (Bean.all in AWA.Counters.Beans.Counter_Bean'Class) then
            ASF.Components.Base.Log_Error (UI, "The bean value is not a Counter_Bean");
            return;
         end if;

         Counter := AWA.Counters.Beans.Counter_Bean'Class (Bean.all)'Unchecked_Access;

         --  Increment the counter associated with the optional key.
         AWA.Counters.Increment (Counter => Counter.Counter,
                                 Key     => Counter.Object);

         --  Render the counter within an optional <span>.
         if Counter.Value >= 0 and then not UI.Is_Hidden (Context) then
            Writer.Start_Optional_Element ("span");
            UI.Render_Attributes (Context, TEXT_ATTRIBUTE_NAMES, Writer);
            Writer.Write_Text (Integer'Image (Counter.Value));
            Writer.End_Optional_Element ("span");
         end if;
      end;
   end Encode_Begin;

begin
   ASF.Utils.Set_Text_Attributes (TEXT_ATTRIBUTE_NAMES);
end AWA.Counters.Components;
