-----------------------------------------------------------------------
--  awa-wikis-beans -- Beans for module wikis
--  Copyright (C) 2015 Stephane Carrez
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
with Ada.Strings.Wide_Wide_Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;

with ADO;
with ADO.Objects;

with ASF.Helpers.Beans;

with AWA.Wikis.Modules;
with AWA.Wikis.Models;
with AWA.Tags.Beans;
with AWA.Counters.Beans;
with AWA.Components.Wikis;

package AWA.Wikis.Beans is

   use Ada.Strings.Wide_Wide_Unbounded;

   type Wiki_Links_Bean is new AWA.Components.Wikis.Link_Renderer_Bean with record
      --  The wiki space identifier.
      Wiki_Space_Id : ADO.Identifier;
   end record;

   --  Get the image link that must be rendered from the wiki image link.
   overriding
   procedure Make_Image_Link (Renderer : in Wiki_Links_Bean;
                              Link     : in Unbounded_Wide_Wide_String;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : out Natural;
                              Height   : out Natural);

   --  Get the page link that must be rendered from the wiki page link.
   overriding
   procedure Make_Page_Link (Renderer : in Wiki_Links_Bean;
                             Link     : in Unbounded_Wide_Wide_String;
                             URI      : out Unbounded_Wide_Wide_String;
                             Exists   : out Boolean);

   type Wiki_View_Bean is new AWA.Wikis.Models.Wiki_View_Info with record
      --  The wiki module instance.
      Module        : Modules.Wiki_Module_Access := null;

      --  The wiki space identifier.
      Wiki_Space_Id : ADO.Identifier;

      --  List of tags associated with the wiki page.
      Tags          : aliased AWA.Tags.Beans.Tag_List_Bean;
      Tags_Bean     : Util.Beans.Basic.Readonly_Bean_Access;

      --  The read page counter associated with the wiki page.
      Counter       : aliased AWA.Counters.Beans.Counter_Bean (Of_Type => ADO.Objects.KEY_INTEGER,
                                                               Of_Class => Models.WIKI_PAGE_TABLE);
      Counter_Bean  : Util.Beans.Basic.Readonly_Bean_Access;

      --  The wiki page links.
      Links         : aliased Wiki_Links_Bean;
      Links_Bean    : Util.Beans.Basic.Readonly_Bean_Access;
   end record;
   type Wiki_View_Bean_Access is access all Wiki_View_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_View_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Wiki_View_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Load the information about the wiki page to display it.
   overriding
   procedure Load (Bean    : in out Wiki_View_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Wiki_View_Bean bean instance.
   function Create_Wiki_View_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access;

   function Get_Wiki_View_Bean is
     new ASF.Helpers.Beans.Get_Bean (Element_Type   => Wiki_View_Bean,
                                     Element_Access => Wiki_View_Bean_Access);

   --  Get a select item list which contains a list of wiki formats.
   function Create_Format_List_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access;

   type Wiki_Space_Bean is new AWA.Wikis.Models.Wiki_Space_Bean with record
      Module    : Modules.Wiki_Module_Access := null;

      --  List of tags associated with the question.
      Tags      : aliased AWA.Tags.Beans.Tag_List_Bean;
      Tags_Bean : Util.Beans.Basic.Readonly_Bean_Access;
   end record;
   type Wiki_Space_Bean_Access is access all Wiki_Space_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_Space_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Wiki_Space_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create or save the wiki space.
   overriding
   procedure Save (Bean    : in out Wiki_Space_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Load the wiki space information.
   overriding
   procedure Load (Bean    : in out Wiki_Space_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Delete the wiki space.
   procedure Delete (Bean    : in out Wiki_Space_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Wiki_Space_Bean bean instance.
   function Create_Wiki_Space_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                  return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Wiki Page Bean
   --  ------------------------------
   --  The <tt>Wiki_Page_Bean</tt> is used to edit a wiki page.  The model type inherit from
   --  the <tt>Wiki_Page</tt> and the wiki page text is hold in the <tt>Content</tt> member.
   --  When a new content is updated, the <tt>Set_Value</tt> procedure sets it in the
   --  <tt>New_Content</tt> member.  It is compared to the current wiki text to decide whether
   --  we have to create a new version or not.
   type Wiki_Page_Bean is new AWA.Wikis.Models.Wiki_Page_Bean with record
      Module     : Modules.Wiki_Module_Access := null;

      --  The page content.
      Content     : Models.Wiki_Content_Ref;
      Has_Content : Boolean := False;
      New_Content : Ada.Strings.Unbounded.Unbounded_String;
      New_Comment : Ada.Strings.Unbounded.Unbounded_String;

      Wiki_Space  : Wiki_Space_Bean;

      --  List of tags associated with the wiki page.
      Tags        : aliased AWA.Tags.Beans.Tag_List_Bean;
      Tags_Bean   : Util.Beans.Basic.Readonly_Bean_Access;
   end record;
   type Wiki_Page_Bean_Access is access all Wiki_Page_Bean'Class;

   --  Returns True if the wiki page has a new text content and requires
   --  a new version to be created.
   function Has_New_Content (Bean : in Wiki_Page_Bean) return Boolean;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_Page_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Wiki_Page_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create or save the wiki page.
   overriding
   procedure Save (Bean    : in out Wiki_Page_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Load the wiki page.
   overriding
   procedure Load (Bean    : in out Wiki_Page_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Delete the wiki page.
   overriding
   procedure Delete (Bean    : in out Wiki_Page_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Wiki_Page_Bean bean instance.
   function Create_Wiki_Page_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Wiki List Bean
   --  ------------------------------
   --  The <b>Wiki_List_Bean</b> gives a list of visible wikis to be displayed to users.
   --  The list can be filtered by a given tag.  The list pagination is supported.
   type Wiki_List_Bean is new AWA.Wikis.Models.Wiki_Page_List_Bean with record
      Module     : Modules.Wiki_Module_Access := null;
      Pages      : aliased AWA.Wikis.Models.Wiki_Page_Info_List_Bean;
      Tags       : AWA.Tags.Beans.Entity_Tag_Map;
      Pages_Bean : AWA.Wikis.Models.Wiki_Page_Info_List_Bean_Access;
   end record;
   type Wiki_List_Bean_Access is access all Wiki_List_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Wiki_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   overriding
   procedure Load (From    : in out Wiki_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Load the list of pages.  If a tag was set, filter the list of pages with the tag.
   procedure Load_List (Into : in out Wiki_List_Bean);

   --  Create the Post_List_Bean bean instance.
   function Create_Wiki_List_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                   return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Wiki Version List Bean
   --  ------------------------------
   type Wiki_Version_List_Bean is new AWA.Wikis.Models.Wiki_Version_List_Bean with record
      Module        : Modules.Wiki_Module_Access := null;
      Versions      : aliased AWA.Wikis.Models.Wiki_Version_Info_List_Bean;
      Versions_Bean : AWA.Wikis.Models.Wiki_Version_Info_List_Bean_Access;
   end record;
   type Wiki_Version_List_Bean_Access is access all Wiki_Version_List_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_Version_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Wiki_Version_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   overriding
   procedure Load (Into    : in out Wiki_Version_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Post_List_Bean bean instance.
   function Create_Wiki_Version_List_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                           return Util.Beans.Basic.Readonly_Bean_Access;

   type Init_Flag is (INIT_WIKI_LIST);
   type Init_Map is array (Init_Flag) of Boolean;

   --  ------------------------------
   --  Admin List Bean
   --  ------------------------------
   --  The <b>Wiki_Admin_Bean</b> is used for the administration of a wiki.  It gives the
   --  list of wikis and pages that are created, published or not.
   type Wiki_Admin_Bean is new Util.Beans.Basic.Bean with record
      Module : AWA.Wikis.Modules.Wiki_Module_Access := null;

      --  The wiki space identifier.
      Wiki_Id          : ADO.Identifier := ADO.NO_IDENTIFIER;

      --  List of blogs.
      Wiki_List        : aliased AWA.Wikis.Models.Wiki_Info_List_Bean;
      Wiki_List_Bean   : AWA.Wikis.Models.Wiki_Info_List_Bean_Access;

      --  Initialization flags.
      Init_Flags       : aliased Init_Map := (others => False);
      Flags            : access Init_Map;
   end record;
   type Wiki_Admin_Bean_Access is access all Wiki_Admin_Bean;

   --  Get the wiki space identifier.
   function Get_Wiki_Id (List : in Wiki_Admin_Bean) return ADO.Identifier;

   overriding
   function Get_Value (List : in Wiki_Admin_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Wiki_Admin_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Load the list of wikis.
   procedure Load_Wikis (List : in Wiki_Admin_Bean);

   --  Create the Wiki_Admin_Bean bean instance.
   function Create_Wiki_Admin_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                    return Util.Beans.Basic.Readonly_Bean_Access;

end AWA.Wikis.Beans;
