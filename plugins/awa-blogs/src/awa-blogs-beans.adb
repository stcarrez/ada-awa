-----------------------------------------------------------------------
--  awa-blogs-beans -- Beans for blog module
--  Copyright (C) 2011 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Util.Strings;
with Util.Dates.ISO8601;
with Util.Beans.Objects.Time;

with Wiki.Helpers;
with Wiki.Documents;
with Wiki.Filters.Autolink;
with Wiki.Filters.Html;
with Wiki.Filters.Collectors;
with Wiki.Render.Text;
with Wiki.Streams.Builders;
with Wiki.Parsers;

with AWA.Images.Modules;
with AWA.Services.Contexts;
with AWA.Helpers.Selectors;
with AWA.Tags.Modules;
with AWA.Comments.Beans;

with ADO.Utils;
with ADO.Queries;
with ADO.SQL;
with ADO.Datasets;
with ADO.Sessions;
with ADO.Sessions.Entities;

package body AWA.Blogs.Beans is

   pragma Wide_Character_Encoding (UTF8);

   use type ADO.Identifier;
   use type Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;

   package ASC renames AWA.Services.Contexts;

   function To_Wide (C : in Character) return Wide_Wide_Character
     renames Ada.Characters.Conversions.To_Wide_Wide_Character;

   --  Sanitize the URI before doing a search in the database.
   --  Ignore every character that we consider to be invalid for the URL.
   function Sanitize_Uri (Uri : in String) return String;

   --  A list of character that we forbid in the URI.  This is used by the
   --  Get_Predefined_Uri to build a default URI from the post title.
   --  The '/' and '+' are allowed.
   Url_Forbidden_Set : constant Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set
     := Ada.Strings.Wide_Wide_Maps.To_Set (Span => (Low  => Wide_Wide_Character'Val (0),
                                          High => ' '))
     or
       Ada.Strings.Wide_Wide_Maps.To_Set (Span => (Low => Wide_Wide_Character'Val (128),
                                         High => Wide_Wide_Character'Last))
     or
       Ada.Strings.Wide_Wide_Maps.To_Set ("?#[]@!$&'""()*,;=%`^\<>");

   --  Translate several UTF-8 accented letters to some Latin-1 equivalents.
   --  The translation is done to build the URL from the title.
   Regular_Map : constant Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Mapping
     := Ada.Strings.Wide_Wide_Maps.To_Mapping
       (From => "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿ",
        To   => "SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyy");

   function Create_From_Format is
     new AWA.Helpers.Selectors.Create_From_Enum (AWA.Blogs.Models.Format_Type,
                                                 "blog_format_");

   --  ------------------------------
   --  Get a select item list which contains a list of blog post formats.
   --  ------------------------------
   function Create_Format_List_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);
      use AWA.Helpers;
   begin
      return Selectors.Create_Selector_Bean (Bundle  => "blogs",
                                             Context => null,
                                             Create  => Create_From_Format'Access).all'Access;
   end Create_Format_List_Bean;

   procedure Make_Image_Link (Renderer : in out Post_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              Info     : in AWA.Blogs.Models.Image_Info;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural) is
      Sep : Natural;
   begin
      Sep := Wiki.Strings.Index (Link, "/");
      URI := Renderer.Image_Prefix;
      Append (URI, Wiki.Strings.To_WString (Util.Strings.Image (Integer (Renderer.Post_Id))));
      Append (URI, "/");
      Append (URI, Wiki.Strings.To_WString (Util.Strings.Image (Integer (Info.Id))));
      Append (URI, "/");
      if Width = 0 and then Height = 0 then
         Append (URI, "default/");
      elsif Width = Natural'Last or else Height = Natural'Last then
         Append (URI, "original/");
      else
         if Width /= 0 then
            Append (URI, Wiki.Strings.To_WString (Util.Strings.Image (Width)));
         end if;
         Append (URI, "x");
         if Height /= 0 then
            Append (URI, Wiki.Strings.To_WString (Util.Strings.Image (Height)));
         end if;
         Append (URI, "/");
      end if;
      if Sep = 0 then
         Append (URI, Link);
      else
         Append (URI, Link (Sep + 1 .. Link'Last));
      end if;
      if Info.Width /= 0 and then Info.Height /= 0 then
         AWA.Images.Modules.Scale (Width     => Info.Width,
                                   Height    => Info.Height,
                                   To_Width  => Width,
                                   To_Height => Height);
      end if;
   end Make_Image_Link;

   procedure Find_Image_Link (Renderer : in out Post_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural) is
      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      List    : AWA.Blogs.Models.Image_Info_Vector;
      Sep     : Natural;
      Query   : ADO.Queries.Context;
      Info    : AWA.Blogs.Models.Image_Info;
   begin
      Sep := Wiki.Strings.Index (Link, "/");
      Query.Bind_Param ("post_id", Renderer.Post_Id);
      if Sep = 0 then
         Query.Bind_Param ("folder_name", String '("Images"));
         Sep := Link'First - 1;
      else
         Query.Bind_Param ("folder_name", Wiki.Strings.To_String (Link (Link'First .. Sep - 1)));
      end if;
      Query.Bind_Param ("file_name", Wiki.Strings.To_String (Link (Sep + 1 .. Link'Last)));
      Query.Set_Query (AWA.Blogs.Models.Query_Blog_Image);
      AWA.Blogs.Models.List (List, Session, Query);
      if not List.Is_Empty then
         Info := List.First_Element;
      else
         Info.Id     := ADO.NO_IDENTIFIER;
         Info.Width  := 0;
         Info.Height := 0;
      end if;
      Renderer.Images.Include (Link, Info);
      Renderer.Make_Image_Link (Link, Info, URI, Width, Height);
   end Find_Image_Link;

   --  ------------------------------
   --  Get the image link that must be rendered from the wiki image link.
   --  ------------------------------
   overriding
   procedure Make_Image_Link (Renderer : in out Post_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural) is
      Pos : Image_Info_Maps.Cursor;
   begin
      if Wiki.Helpers.Is_Url (Link) or else Link (Link'First) = '/' then
         URI    := To_Unbounded_Wide_Wide_String (Link);
      else
         Pos := Renderer.Images.Find (Link);
         if Image_Info_Maps.Has_Element (Pos) then
            Renderer.Make_Image_Link (Link, Image_Info_Maps.Element (Pos), URI, Width, Height);
         else
            Renderer.Find_Image_Link (Link, URI, Width, Height);
         end if;
      end if;
   end Make_Image_Link;

   --  ------------------------------
   --  Get the page link that must be rendered from the wiki page link.
   --  ------------------------------
   overriding
   procedure Make_Page_Link (Renderer : in out Post_Links_Bean;
                             Link     : in Wiki.Strings.WString;
                             URI      : out Unbounded_Wide_Wide_String;
                             Exists   : out Boolean) is
   begin
      if Wiki.Helpers.Is_Url (Link) then
         URI := To_Unbounded_Wide_Wide_String (Link);
      else
         URI := Renderer.Page_Prefix;
         Append (URI, Link);
      end if;
      Exists := True;
   end Make_Page_Link;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Blog_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if not From.Is_Null then
         return AWA.Blogs.Models.Blog_Ref (From).Get_Value (Name);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Blog_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" and then not Util.Beans.Objects.Is_Empty (Value) then
         From.Set_Id (ADO.Utils.To_Identifier (Value));
      else
         AWA.Blogs.Models.Blog_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create a new blog.
   --  ------------------------------
   overriding
   procedure Create (Bean    : in out Blog_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      Result  : ADO.Identifier;
   begin
      Bean.Module.Create_Blog (Title        => Bean.Get_Name,
                               Result       => Result);
      Bean.Set_Id (Result);
   end Create;

   --  ------------------------------
   --  Load the blog information.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Blog_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Session : ADO.Sessions.Session := Bean.Module.Get_Session;
      Found   : Boolean;
      Query   : ADO.SQL.Query;
   begin
      if not Bean.Is_Null and then Bean.Get_Id /= ADO.NO_IDENTIFIER then
         Query.Bind_Param (1, Bean.Get_Id);
         Query.Set_Filter ("o.id = ?");
         Bean.Find (Session, Query, Found);
      else
         Found := False;
      end if;
      if not Found then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
      else
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");
      end if;
   end Load;

   --  ------------------------------
   --  Handle an event to create the blog entry automatically.
   --  ------------------------------
   overriding
   procedure Create_Default (Bean    : in out Blog_Bean;
                             Event   : in AWA.Events.Module_Event'Class) is
      pragma Unreferenced (Event);

      Result  : ADO.Identifier;
   begin
      Bean.Module.Create_Blog (Title        => Bean.Get_Name,
                               Result       => Result);
   end Create_Default;

   --  ------------------------------
   --  Create the Blog_Bean bean instance.
   --  ------------------------------
   function Create_Blog_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access is

      Object  : constant Blog_Bean_Access := new Blog_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Blog_Bean;

   --  ------------------------------
   --  Build the URI from the post title and the post date.
   --  ------------------------------
   function Get_Predefined_Uri (Title : in String;
                                Date  : in Ada.Calendar.Time) return String is
      D      : constant String := Util.Dates.ISO8601.Image (Date);
      C      : Wide_Wide_Character;
      S      : constant Wide_Wide_String
        := Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (Title);
      Result : String (1 .. S'Length + 11);
      Len    : Positive := 11;
   begin
      Result (1 .. 4) := D (1 .. 4);
      Result (5) := '/';
      Result (6 .. 7) := D (6 .. 7);
      Result (8) := '/';
      Result (9 .. 10) := D (9 .. 10);
      Result (11) := '/';
      Len := 11;
      for I in S'Range loop
         C := Ada.Strings.Wide_Wide_Maps.Value (Regular_Map, S (I));
         if not Ada.Strings.Wide_Wide_Maps.Is_In (C, Url_Forbidden_Set) then
            Len := Len + 1;
            Result (Len) := Ada.Characters.Conversions.To_Character (C);
         elsif Result (Len) /= '-' then
            Len := Len + 1;
            Result (Len) := '-';
         end if;
      end loop;
      return Result (Result'First .. Len);
   end Get_Predefined_Uri;

   --  ------------------------------
   --  Create or save the post.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Post_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      Result  : ADO.Identifier;
   begin
      if String '(Bean.Get_Uri) = "" then
         Bean.Set_Uri (Get_Predefined_Uri (Bean.Get_Title, Ada.Calendar.Clock));
      end if;
      if not Bean.Is_Inserted then
         Bean.Module.Create_Post (Blog_Id => Bean.Blog_Id,
                                  Title   => Bean.Get_Title,
                                  URI     => Bean.Get_Uri,
                                  Text    => Bean.Get_Text,
                                  Summary => Bean.Get_Summary,
                                  Format  => Bean.Get_Format,
                                  Comment => Bean.Get_Allow_Comments,
                                  Status  => Bean.Get_Status,
                                  Result  => Result);
         Bean.Set_Id (Result);
      else
         Bean.Module.Update_Post (Post_Id => Bean.Get_Id,
                                  Title   => Bean.Get_Title,
                                  URI     => Bean.Get_Uri,
                                  Text    => Bean.Get_Text,
                                  Summary => Bean.Get_Summary,
                                  Format  => Bean.Get_Format,
                                  Comment => Bean.Get_Allow_Comments,
                                  Publish_Date => Bean.Get_Publish_Date,
                                  Status  => Bean.Get_Status);
         Result := Bean.Get_Id;
      end if;
      Bean.Tags.Update_Tags (Result);
   end Save;

   --  ------------------------------
   --  Create or save the post and publish it.
   --  ------------------------------
   overriding
   procedure Publish (Bean    : in out Post_Bean;
                      Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Set_Status (Models.POST_PUBLISHED);
      Bean.Save (Outcome);
   end Publish;

   --  ------------------------------
   --  Delete a post.
   --  ------------------------------
   overriding
   procedure Delete (Bean    : in out Post_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Delete_Post (Post_Id => Bean.Get_Id);
   end Delete;

   --  ------------------------------
   --  Sanitize the URI before doing a search in the database.
   --  Ignore every character that we consider to be invalid for the URI.
   --  ------------------------------
   function Sanitize_Uri (Uri : in String) return String is
      Result : String (1 .. Uri'Length);
      Pos    : Natural := 1;
   begin
      for I in Uri'Range loop
         if not Ada.Strings.Wide_Wide_Maps.Is_In (To_Wide (Uri (I)), Url_Forbidden_Set) then
            Result (Pos) := Uri (I);
            Pos := Pos + 1;
         end if;
      end loop;
      return Result (1 .. Pos - 1);
   end Sanitize_Uri;

   --  ------------------------------
   --  Make the post description from the summary or the content.
   --  ------------------------------
   procedure Make_Description (From : in out Post_Bean) is

      procedure Collect_Image (Pos : in Wiki.Filters.Collectors.Cursor);

      Doc      : Wiki.Documents.Document;
      Engine   : Wiki.Parsers.Parser;
      Autolink : aliased Wiki.Filters.Autolink.Autolink_Filter;
      Renderer : aliased Wiki.Render.Text.Text_Renderer;
      Filter   : aliased Wiki.Filters.Html.Html_Filter_Type;
      Format   : Wiki.Wiki_Syntax;
      Images   : aliased Wiki.Filters.Collectors.Image_Collector_Type;
      Stream   : aliased Wiki.Streams.Builders.Output_Builder_Stream;
      Summary  : constant String := From.Get_Summary;

      procedure Collect_Image (Pos : in Wiki.Filters.Collectors.Cursor) is
         Image : constant Wiki.Strings.WString
           := Wiki.Filters.Collectors.WString_Maps.Key (Pos);
         W, H  : Natural := 0;
      begin
         if Length (From.Image_Link) = 0 then
            From.Links.Make_Image_Link (Link   => Image,
                                        URI    => From.Image_Link,
                                        Width  => W,
                                        Height => H);
         end if;
      end Collect_Image;

   begin
      case From.Get_Format is
         when Models.FORMAT_DOTCLEAR =>
            Format := Wiki.SYNTAX_DOTCLEAR;

         when Models.FORMAT_HTML =>
            Format := Wiki.SYNTAX_HTML;

         when Models.FORMAT_MARKDOWN =>
            Format := Wiki.SYNTAX_MARKDOWN;

         when Models.FORMAT_MEDIAWIKI =>
            Format := Wiki.SYNTAX_MEDIA_WIKI;

         when Models.FORMAT_CREOLE =>
            Format := Wiki.SYNTAX_CREOLE;

      end case;
      Engine.Add_Filter (Autolink'Unchecked_Access);
      Engine.Add_Filter (Images'Unchecked_Access);
      Engine.Add_Filter (Filter'Unchecked_Access);
      Engine.Set_Syntax (Format);
      if Summary'Length > 0 then
         Engine.Parse (Summary, Doc);
      else
         Engine.Parse (From.Get_Text, Doc);
      end if;
      Renderer.Set_Output_Stream (Stream'Unchecked_Access);
      Renderer.Set_No_Newline (True);
      Renderer.Render (Doc);
      From.Description := Ada.Strings.Unbounded.To_Unbounded_String (Stream.To_String);
      Images.Iterate (Collect_Image'Access);

      --  No image in the summary, scan the main contain to get one if possible.
      if Summary'Length > 0 and then Length (From.Image_Link) = 0 then
         Engine.Parse (From.Get_Text, Doc);
         Images.Iterate (Collect_Image'Access);
      end if;
   end Make_Description;

   --  ------------------------------
   --  Load the post from the URI either with visible comments or with all comments.
   --  ------------------------------
   procedure Load (Bean         : in out Post_Bean;
                   Outcome      : in out Ada.Strings.Unbounded.Unbounded_String;
                   Publish_Only : in Boolean) is
      use type AWA.Comments.Beans.Comment_Bean_Access;
      use type AWA.Comments.Beans.Comment_List_Bean_Access;
      use Wiki.Strings;

      Session      : ADO.Sessions.Session := Bean.Module.Get_Session;
      Query        : ADO.SQL.Query;
      Found        : Boolean;
      Comment      : AWA.Comments.Beans.Comment_Bean_Access;
      Comment_List : AWA.Comments.Beans.Comment_List_Bean_Access;
   begin
      if not Bean.Is_Null and then Bean.Get_Id /= ADO.NO_IDENTIFIER then
         Query.Bind_Param (1, Bean.Get_Id);
         Query.Set_Filter ("o.id = ?");
      else
         Query.Bind_Param (1, Sanitize_Uri (String '(Bean.Get_Uri)));
         Query.Set_Filter ("o.uri = ?");
      end if;
      Bean.Find (Session, Query, Found);
      if not Found then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
         return;
      end if;
      Bean.Tags.Load_Tags (Session, Bean.Get_Id);

      Bean.Counter.Value := Bean.Get_Read_Count;
      ADO.Objects.Set_Value (Bean.Counter.Object, Bean.Get_Id);
      Bean.Links.Post_Id := Bean.Get_Id;

      Comment := AWA.Comments.Beans.Get_Comment_Bean ("postNewComment");
      if Comment /= null then
         Comment.Set_Entity_Id (Bean.Get_Id);
      end if;
      Comment_List := AWA.Comments.Beans.Get_Comment_List_Bean ("postComments");
      if Comment_List /= null then
         Comment_List.Publish_Only := Publish_Only;
         Comment_List.Load_Comments (Bean.Get_Id);
      end if;

      Make_Description (Bean);
      if Wiki.Strings.Length (Bean.Image_Link) = 0 then
         Bean.Image_Link := To_UString (To_WString (Bean.Get_Blog.Get_Default_Image_Url));
      end if;
   end Load;

   --  ------------------------------
   --  Load the post from the URI for the public display.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Post_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Load (Outcome, True);
   end Load;

   --  ------------------------------
   --  Setup the bean to create a new post.
   --  ------------------------------
   overriding
   procedure Setup (Bean    : in out Post_Bean;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      Blog : constant Blog_Bean_Access := Get_Blog_Bean ("blog");
   begin
      if Blog = null or else Bean.Blog_Id = ADO.NO_IDENTIFIER then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("not-found");
         return;
      end if;
      Blog.Set_Id (Bean.Blog_Id);
      Blog.Load (Outcome);
      Bean.Set_Format (Blog.Get_Format);
   end Setup;

   --  ------------------------------
   --  Load the post from the URI for the administrator.
   --  ------------------------------
   overriding
   procedure Load_Admin (Bean    : in out Post_Bean;
                         Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Load (Outcome, False);
   end Load_Admin;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Post_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = BLOG_ID_ATTR then
         return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Blog_Id));
      elsif Name = POST_TAG_ATTR then
         return Util.Beans.Objects.To_Object (From.Tags_Bean, Util.Beans.Objects.STATIC);
      elsif Name = COUNTER_ATTR then
         return Util.Beans.Objects.To_Object (From.Counter_Bean, Util.Beans.Objects.STATIC);
      elsif From.Is_Null then
         if Name = POST_ALLOW_COMMENTS_ATTR then
            return Util.Beans.Objects.To_Object (False);
         else
            return Util.Beans.Objects.Null_Object;
         end if;
      elsif Name = POST_ID_ATTR then
         if From.Get_Id /= ADO.NO_IDENTIFIER then
            return Util.Beans.Objects.To_Object (Long_Long_Integer (From.Get_Id));
         else
            return Util.Beans.Objects.Null_Object;
         end if;
      elsif Name = POST_USERNAME_ATTR then
         return Util.Beans.Objects.To_Object (String '(From.Get_Author.Get_Name));
      elsif Name = POST_DESCRIPTION_ATTR then
         return Util.Beans.Objects.To_Object (From.Description);
      elsif Name = POST_IMAGE_ATTR then
         return Util.Beans.Objects.To_Object (From.Image_Link);
      elsif Name = "links" then
         return Util.Beans.Objects.To_Object (From.Links_Bean, Util.Beans.Objects.STATIC);
      else
         return AWA.Blogs.Models.Post_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Post_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = BLOG_ID_ATTR then
         From.Blog_Id := ADO.Utils.To_Identifier (Value);
      elsif Name = POST_ID_ATTR and then not Util.Beans.Objects.Is_Empty (Value) then
         From.Load_Post (ADO.Utils.To_Identifier (Value));
      elsif Name = POST_UID_ATTR and then not Util.Beans.Objects.Is_Empty (Value) then
         From.Set_Id (ADO.Utils.To_Identifier (Value));
      else
         AWA.Blogs.Models.Post_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Find a plugin knowing its name.
   --  ------------------------------
   overriding
   function Find (Factory : in Post_Bean;
                  Name    : in String) return Wiki.Plugins.Wiki_Plugin_Access is
   begin
      if Name = "if" or else Name = "else" or else Name = "elsif" or else Name = "end" then
         return Factory.Condition'Unrestricted_Access;
      elsif Name = "set" then
         return Factory.Variable'Unrestricted_Access;
      elsif Name = "list" then
         return Factory.List_Variable'Unrestricted_Access;
      else
         return null;
      end if;
   end Find;

   --  ------------------------------
   --  Load the post.
   --  ------------------------------
   procedure Load_Post (Post : in out Post_Bean;
                        Id   : in ADO.Identifier) is
      Session : ADO.Sessions.Session := Post.Module.Get_Session;
   begin
      Post.Load (Session, Id);
      Post.Tags.Load_Tags (Session, Id);
      Post.Counter.Value := Post.Get_Read_Count;
      ADO.Objects.Set_Value (Post.Counter.Object, Id);
   end Load_Post;

   --  ------------------------------
   --  Create the Workspaces_Bean bean instance.
   --  ------------------------------
   function Create_Post_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Post_Bean_Access := new Post_Bean;
   begin
      Object.Module := Module;
      Object.Tags_Bean := Object.Tags'Access;
      Object.Tags.Set_Entity_Type (AWA.Blogs.Models.POST_TABLE);
      Object.Tags.Set_Permission ("blog-update-post");
      Object.Counter_Bean := Object.Counter'Access;
      Object.Counter.Counter := AWA.Blogs.Modules.Read_Counter.Index;
      Object.Links_Bean := Object.Links'Access;
      Object.Links.Image_Prefix := Module.Get_Image_Prefix;
      return Object.all'Access;
   end Create_Post_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Post_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      Pos : Natural;
   begin
      if Name = "tags" then
         Pos := From.Posts.Get_Row_Index;
         if Pos = 0 then
            return Util.Beans.Objects.Null_Object;
         end if;
         declare
            Item : constant Models.Post_Info := From.Posts.List.Element (Pos);
         begin
            return From.Tags.Get_Tags (Item.Id);
         end;
      elsif Name = "posts" then
         return Util.Beans.Objects.To_Object (Value   => From.Posts_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      elsif Name = COUNTER_ATTR then
         Pos := From.Posts.Get_Row_Index;
         if Pos = 0 then
            return Util.Beans.Objects.Null_Object;
         end if;
         declare
            Item : constant Models.Post_Info := From.Posts.List.Element (Pos);
         begin
            ADO.Objects.Set_Value (From.Counter_Bean.Object, Item.Id);
         end;
         return Util.Beans.Objects.To_Object (From.Counter_Bean, Util.Beans.Objects.STATIC);

      elsif Name = "tag" then
         return Util.Beans.Objects.To_Object (From.Tag);

      elsif Name = "page" then
         return Util.Beans.Objects.To_Object (From.Page);

      elsif Name = "count" then
         return Util.Beans.Objects.To_Object (From.Count);

      elsif Name = "page_count" then
         return Util.Beans.Objects.To_Object ((From.Count + From.Page_Size - 1) / From.Page_Size);

      elsif Name = "links" then
         Pos := From.Posts.Get_Row_Index;
         if Pos = 0 then
            return Util.Beans.Objects.Null_Object;
         end if;
         declare
            Item : constant Models.Post_Info := From.Posts.List.Element (Pos);
         begin
            From.Links_Bean.Post_Id := Item.Id;
            return Util.Beans.Objects.To_Object (From.Links_Bean, Util.Beans.Objects.STATIC);
         end;

      elsif Name = "updateDate" then
         if From.Posts.Get_Count = 0 then
            return Util.Beans.Objects.Null_Object;
         else
            declare
               Item : constant Models.Post_Info := From.Posts.List.Element (1);
            begin
               return Util.Beans.Objects.Time.To_Object (Item.Date);
            end;
         end if;
      else
         return From.Posts.Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Post_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name /= "page" or else not Util.Beans.Objects.Is_Empty (Value) then
         AWA.Blogs.Models.Post_List_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Load the list of posts.  If a tag was set, filter the list of posts with the tag.
   --  ------------------------------
   procedure Load_List (Into : in out Post_List_Bean) is
      use AWA.Blogs.Models;

      Session     : ADO.Sessions.Session := Into.Service.Get_Session;
      Query       : ADO.Queries.Context;
      Count_Query : ADO.Queries.Context;
      Tag_Id      : ADO.Identifier;
      First       : constant Natural  := (Into.Page - 1) * Into.Page_Size;
   begin
      AWA.Tags.Modules.Find_Tag_Id (Session, Ada.Strings.Unbounded.To_String (Into.Tag), Tag_Id);
      if Tag_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (AWA.Blogs.Models.Query_Blog_Post_Tag_List);
         Query.Bind_Param (Name => "tag", Value => Tag_Id);
         Count_Query.Set_Count_Query (AWA.Blogs.Models.Query_Blog_Post_Tag_List);
         Count_Query.Bind_Param (Name => "tag", Value => Tag_Id);
      else
         Query.Set_Query (AWA.Blogs.Models.Query_Blog_Post_List);
         Count_Query.Set_Count_Query (AWA.Blogs.Models.Query_Blog_Post_List);
      end if;
      Query.Bind_Param (Name => "first", Value => First);
      Query.Bind_Param (Name => "count", Value => Into.Page_Size);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "entity_type",
                                        Table   => AWA.Blogs.Models.POST_TABLE,
                                        Session => Session);
      ADO.Sessions.Entities.Bind_Param (Params  => Count_Query,
                                        Name    => "entity_type",
                                        Table   => AWA.Blogs.Models.POST_TABLE,
                                        Session => Session);
      AWA.Blogs.Models.List (Into.Posts, Session, Query);
      Into.Count := ADO.Datasets.Get_Count (Session, Count_Query);
      declare
         List : ADO.Utils.Identifier_Vector;
         Iter : Post_Info_Vectors.Cursor := Into.Posts.List.First;
      begin
         while Post_Info_Vectors.Has_Element (Iter) loop
            List.Append (Post_Info_Vectors.Element (Iter).Id);
            Post_Info_Vectors.Next (Iter);
         end loop;
         Into.Tags.Load_Tags (Session, AWA.Blogs.Models.POST_TABLE.Table.all,
                              List);
      end;
   end Load_List;

   overriding
   procedure Load (From    : in out Post_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      From.Load_List;
   end Load;

   --  ------------------------------
   --  Create the Post_List_Bean bean instance.
   --  ------------------------------
   function Create_Post_List_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Post_List_Bean_Access := new Post_List_Bean;
   begin
      Object.Service    := Module;
      Object.Posts_Bean := Object.Posts'Access;
      Object.Page_Size  := 20;
      Object.Page       := 1;
      Object.Count      := 0;
      Object.Links_Bean := Object.Links'Access;
      Object.Links.Image_Prefix := Module.Get_Image_Prefix;
      Object.Counter_Bean := Object.Counter'Access;
      Object.Counter.Counter := AWA.Blogs.Modules.Read_Counter.Index;
      return Object.all'Access;
   end Create_Post_List_Bean;

   --  ------------------------------
   --  Create the Blog_List_Bean bean instance.
   --  ------------------------------
   function Create_Blog_Admin_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Blog_Admin_Bean_Access := new Blog_Admin_Bean;
   begin
      Object.Module            := Module;
      Object.Flags             := Object.Init_Flags'Access;
      Object.Post_List_Bean    := Object.Post_List'Access;
      Object.Blog_List_Bean    := Object.Blog_List'Access;
      Object.Comment_List_Bean := Object.Comment_List'Access;
      return Object.all'Access;
   end Create_Blog_Admin_Bean;

   function Create_From_Status is
     new AWA.Helpers.Selectors.Create_From_Enum (AWA.Blogs.Models.Post_Status_Type,
                                                 "blog_status_");

   --  ------------------------------
   --  Get a select item list which contains a list of post status.
   --  ------------------------------
   function Create_Status_List (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);
      use AWA.Helpers;
   begin
      return Selectors.Create_Selector_Bean (Bundle  => "blogs",
                                             Context => null,
                                             Create  => Create_From_Status'Access).all'Access;
   end Create_Status_List;

   --  ------------------------------
   --  Load the list of blogs.
   --  ------------------------------
   procedure Load_Blogs (List : in Blog_Admin_Bean) is
      use AWA.Blogs.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
   begin
      Query.Set_Query (AWA.Blogs.Models.Query_Blog_List);
      Query.Bind_Param ("user_id", User);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "table",
                                        Table   => AWA.Blogs.Models.BLOG_TABLE,
                                        Session => Session);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "entity_type",
                                        Table   => AWA.Blogs.Models.POST_TABLE,
                                        Session => Session);
      AWA.Blogs.Models.List (List.Blog_List_Bean.all, Session, Query);
      List.Flags (INIT_BLOG_LIST) := True;
   end Load_Blogs;

   --  ------------------------------
   --  Get the blog identifier.
   --  ------------------------------
   function Get_Blog_Id (List : in Blog_Admin_Bean) return ADO.Identifier is
   begin
      if List.Blog_Id = ADO.NO_IDENTIFIER then
         if not List.Flags (INIT_BLOG_LIST) then
            Load_Blogs (List);
         end if;
         if not List.Blog_List.List.Is_Empty then
            return List.Blog_List.List.Element (1).Id;
         end if;
      end if;
      return List.Blog_Id;
   end Get_Blog_Id;

   --  ------------------------------
   --  Load the posts associated with the current blog.
   --  ------------------------------
   procedure Load_Posts (List : in Blog_Admin_Bean) is
      use AWA.Blogs.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
      Blog_Id : constant ADO.Identifier := List.Get_Blog_Id;
   begin
      if Blog_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (AWA.Blogs.Models.Query_Blog_Admin_Post_List);
         Query.Bind_Param ("blog_id", Blog_Id);
         Query.Bind_Param ("user_id", User);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "table",
                                           Table   => AWA.Blogs.Models.BLOG_TABLE,
                                           Session => Session);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "entity_type",
                                           Table   => AWA.Blogs.Models.POST_TABLE,
                                           Session => Session);

         AWA.Blogs.Models.List (List.Post_List_Bean.all, Session, Query);
         List.Flags (INIT_POST_LIST) := True;
      end if;
   end Load_Posts;

   --  ------------------------------
   --  Load the comments associated with the current blog.
   --  ------------------------------
   procedure Load_Comments (List : in Blog_Admin_Bean) is
      use AWA.Blogs.Models;
      use AWA.Services;

      Ctx     : constant Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
      Blog_Id : constant ADO.Identifier := List.Get_Blog_Id;
   begin
      if Blog_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (AWA.Blogs.Models.Query_Comment_List);
         Query.Bind_Param ("blog_id", Blog_Id);
         Query.Bind_Param ("user_id", User);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "table",
                                           Table   => AWA.Blogs.Models.BLOG_TABLE,
                                           Session => Session);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "entity_type",
                                           Table   => AWA.Blogs.Models.POST_TABLE,
                                           Session => Session);

         AWA.Blogs.Models.List (List.Comment_List_Bean.all, Session, Query);
         List.Flags (INIT_COMMENT_LIST) := True;
      end if;
   end Load_Comments;

   overriding
   function Get_Value (List : in Blog_Admin_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "blogs" then
         if not List.Init_Flags (INIT_BLOG_LIST) then
            Load_Blogs (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Blog_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "posts" then
         if not List.Init_Flags (INIT_POST_LIST) then
            Load_Posts (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Post_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "comments" then
         if not List.Init_Flags (INIT_COMMENT_LIST) then
            Load_Comments (List);
         end if;
         return Util.Beans.Objects.To_Object (Value   => List.Comment_List_Bean,
                                              Storage => Util.Beans.Objects.STATIC);

      elsif Name = "id" then
         declare
            Id : constant ADO.Identifier := List.Get_Blog_Id;
         begin
            if Id = ADO.NO_IDENTIFIER then
               return Util.Beans.Objects.Null_Object;
            else
               return Util.Beans.Objects.To_Object (Long_Long_Integer (Id));
            end if;
         end;
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Blog_Admin_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" and then not Util.Beans.Objects.Is_Empty (Value) then
         From.Blog_Id := ADO.Utils.To_Identifier (Value);
      end if;
   end Set_Value;

   overriding
   function Get_Value (List : in Blog_Stat_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "stats" then
         return Util.Beans.Objects.To_Object (Value   => List.Stats_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      else
         return AWA.Blogs.Models.Stat_List_Bean (List).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Blog_Stat_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if not Util.Beans.Objects.Is_Empty (Value) then
         AWA.Blogs.Models.Stat_List_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Load the statistics information.
   --  ------------------------------
   overriding
   procedure Load (List    : in out Blog_Stat_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      Ctx     : constant ASC.Service_Context_Access := ASC.Current;
      User    : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session : ADO.Sessions.Session := ASC.Get_Session (Ctx);
      Query   : ADO.Queries.Context;
   begin
      if List.Blog_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (AWA.Blogs.Models.Query_Post_Publish_Stats);
         Query.Bind_Param ("blog_id", List.Blog_Id);
         Query.Bind_Param ("user_id", User);
         ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                           Name    => "table",
                                           Table   => AWA.Blogs.Models.BLOG_TABLE,
                                           Session => Session);

         AWA.Blogs.Models.List (List.Stats, Session, Query);
      end if;
   end Load;

   --  ------------------------------
   --  Create the Blog_Stat_Bean bean instance.
   --  ------------------------------
   function Create_Blog_Stat_Bean (Module : in AWA.Blogs.Modules.Blog_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Blog_Stat_Bean_Access := new Blog_Stat_Bean;
   begin
      Object.Module     := Module;
      Object.Stats_Bean := Object.Stats'Access;
      Object.Post_Id    := ADO.NO_IDENTIFIER;
      Object.Blog_Id    := ADO.NO_IDENTIFIER;
      return Object.all'Access;
   end Create_Blog_Stat_Bean;

end AWA.Blogs.Beans;
