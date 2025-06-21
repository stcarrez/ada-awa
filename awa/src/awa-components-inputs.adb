-----------------------------------------------------------------------
--  awa-components-inputs -- AWA input field component
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with ASF.Components.Html.Messages;
with ASF.Applications.Messages.Vectors;

with Util.Beans.Objects;
package body AWA.Components.Inputs is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Render the input field title.
   --  ------------------------------
   procedure Render_Title (UI      : in UIInput;
                           Writer  : in Response_Writer_Access;
                           Context : in out Faces_Context'Class) is
      Title : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "title");
   begin
      Writer.Start_Element ("dt");
      Writer.Write (Title);
      Writer.End_Element ("dt");
   end Render_Title;

   --  ------------------------------
   --  Render the input component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIInput;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Id       : constant String := To_String (UI.Get_Client_Id);
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Messages : constant ASF.Applications.Messages.Vectors.Cursor := Context.Get_Messages (Id);
         Style    : constant String := UI.Get_Attribute ("styleClass", Context);
      begin
         Writer.Start_Element ("dl");
         if ASF.Applications.Messages.Vectors.Has_Element (Messages) then
            Writer.Write_Attribute ("class", Style & " awa-error");
         elsif Style'Length > 0 then
            Writer.Write_Attribute ("class", Style);
         end if;
         UI.Render_Title (Writer, Context);
         Writer.Start_Element ("dd");
         UI.Render_Input (Context);
      end;
   end Encode_Begin;

   --  ------------------------------
   --  Render the end of the input component.  Closes the DL/DD list.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIInput;
                         Context : in out Faces_Context'Class) is
      use ASF.Components.Html.Messages;
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      --  Render the error message associated with the input field.
      declare
         Id       : constant String := To_String (UI.Get_Client_Id);
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Messages : constant ASF.Applications.Messages.Vectors.Cursor := Context.Get_Messages (Id);
      begin
         if ASF.Applications.Messages.Vectors.Has_Element (Messages) then
            Write_Message (UI, ASF.Applications.Messages.Vectors.Element (Messages),
                           SPAN_NO_STYLE, False, True, Context);
         end if;
         Writer.End_Element ("dd");
         Writer.End_Element ("dl");
      end;
   end Encode_End;

end AWA.Components.Inputs;
