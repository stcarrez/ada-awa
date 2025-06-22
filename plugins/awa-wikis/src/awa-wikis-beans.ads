-----------------------------------------------------------------------
--  awa-wikis-beans -- Beans for module wikis
--  Copyright (C) 2015, 2016, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

with Util.Beans.Basic;
with Util.Beans.Objects;

with ADO;
with ADO.Objects;

with ASF.Helpers.Beans;

with Wiki.Strings;
with Wiki.Attributes;
with Wiki.Plugins.Templates;
with Wiki.Plugins.Conditions;
with Wiki.Plugins.Variables;

with AWA.Wikis.Modules;
with AWA.Wikis.Models;
with AWA.Tags.Beans;
with AWA.Counters.Beans;
with AWA.Components.Wikis;

--  == Ada Beans ==
--  Several bean types are provided to represent and manage the blogs and their posts.
--  The blog module registers the bean constructors when it is initialized.
--  To use them, one must declare a bean definition in the application XML configuration.
--
--  @include-bean wikis.xml
--  @include-bean wiki-page.xml
--  @include-bean wiki-pages.xml
--  @include-bean wiki-history.xml
--  @include-bean wiki-list.xml
package AWA.Wikis.Beans is

   use Ada.Strings.Wide_Wide_Unbounded;

   package Image_Info_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => Wiki.Strings.WString,
                                                Element_Type    => Wikis.Models.Wiki_Image_Info,
                                                Hash            => Ada.Strings.Wide_Wide_Hash,
                                                Equivalent_Keys => "=",
                                                "="             => AWA.Wikis.Models."=");

   package Template_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Wiki.Strings.UString,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=",
                                                "="             => "=");

   type Wiki_Links_Bean is new AWA.Components.Wikis.Link_Renderer_Bean
     and Util.Beans.Basic.List_Bean with record
      --  The wiki space identifier.
      Wiki_Space_Id : ADO.Identifier;
      Images        : Image_Info_Maps.Map;

      --  The info bean used for the list iterator.
      Info          : aliased AWA.Wikis.Models.Wiki_Image_Info;
      Info_Bean     : Util.Beans.Basic.Readonly_Bean_Access;

      --  Current index when iterating over the list.
      Pos           : Image_Info_Maps.Cursor;
   end record;

   procedure Make_Image_Link (Renderer : in out Wiki_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              Info     : in AWA.Wikis.Models.Wiki_Image_Info;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural);

   procedure Find_Image_Link (Renderer : in out Wiki_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural);

   --  Get the image link that must be rendered from the wiki image link.
   overriding
   procedure Make_Image_Link (Renderer : in out Wiki_Links_Bean;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : in out Natural;
                              Height   : in out Natural);

   --  Get the page link that must be rendered from the wiki page link.
   overriding
   procedure Make_Page_Link (Renderer : in out Wiki_Links_Bean;
                             Link     : in Wiki.Strings.WString;
                             URI      : out Unbounded_Wide_Wide_String;
                             Exists   : out Boolean);

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_Links_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : Wiki_Links_Bean) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out Wiki_Links_Bean;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in Wiki_Links_Bean) return Util.Beans.Objects.Object;

   --  The Wiki template plugin that retrieves the template content from the Wiki space.
   type Wiki_Template_Bean is new Wiki.Plugins.Templates.Template_Plugin
     and Wiki.Plugins.Plugin_Factory
     and Util.Beans.Basic.List_Bean with record

      --  The wiki space identifier.
      Wiki_Space_Id : ADO.Identifier;

      --  The list of templates that have been loaded.
      Templates     : Template_Maps.Map;

      --  Condition plugin.
      Condition     : aliased Wiki.Plugins.Conditions.Condition_Plugin;

      --  Variable plugin.
      Variable      : aliased Wiki.Plugins.Variables.Variable_Plugin;
      List_Variable : aliased Wiki.Plugins.Variables.List_Variable_Plugin;

      --  The info bean used for the list iterator.
      Info          : aliased AWA.Wikis.Models.Wiki_Info;
      Info_Bean     : Util.Beans.Basic.Readonly_Bean_Access;

      --  Current index when iterating over the list.
      Pos           : Template_Maps.Cursor;
   end record;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_Template_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : Wiki_Template_Bean) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out Wiki_Template_Bean;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in Wiki_Template_Bean) return Util.Beans.Objects.Object;

   --  Get the template content for the plugin evaluation.
   overriding
   procedure Get_Template (Plugin   : in out Wiki_Template_Bean;
                           Params   : in out Wiki.Attributes.Attribute_List;
                           Template : out Wiki.Strings.UString);

   --  Find a plugin knowing its name.
   overriding
   function Find (Factory : in Wiki_Template_Bean;
                  Name    : in String) return Wiki.Plugins.Wiki_Plugin_Access;

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

   function Get_Wiki_Space_Bean is
     new ASF.Helpers.Beans.Get_Bean (Element_Type   => Wiki_Space_Bean,
                                     Element_Access => Wiki_Space_Bean_Access);

   type Wiki_View_Bean is new AWA.Wikis.Models.Wiki_View_Info with record
      --  The wiki module instance.
      Module        : Modules.Wiki_Module_Access := null;

      --  The wiki space identifier.
      Wiki_Space    : Wiki_Space_Bean_Access;

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

      --  The wiki plugins.
      Plugins       : aliased Wiki_Template_Bean;
      Plugins_Bean  : Util.Beans.Basic.Readonly_Bean_Access;
   end record;
   type Wiki_View_Bean_Access is access all Wiki_View_Bean'Class;

   --  Set the wiki identifier.
   procedure Set_Wiki_Id (Into : in out Wiki_View_Bean;
                          Id   : in ADO.Identifier);

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_View_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Wiki_View_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Get the wiki syntax for the page.
   function Get_Syntax (From : in Wiki_View_Bean) return Wiki.Wiki_Syntax;

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
      Format      : AWA.Wikis.Models.Format_Type := AWA.Wikis.Models.FORMAT_CREOLE;
      New_Content : Ada.Strings.Unbounded.Unbounded_String;
      New_Comment : Ada.Strings.Unbounded.Unbounded_String;

      Wiki_Space  : Wiki_Space_Bean_Access;

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

   --  Setup the wiki page for the creation.
   overriding
   procedure Setup (Bean    : in out Wiki_Page_Bean;
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

      --  The wiki space identifier.
      Wiki_Space : Wiki_Space_Bean_Access;
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

   --  ------------------------------
   --  Wiki page info Bean
   --  ------------------------------
   --  The <tt>Wiki_Page_Info_Bean</tt> is used to provide information about a wiki page.
   --  It analyzes the page content and extract the list of links, images, words, templates
   --  used in the page.
   type Wiki_Page_Info_Bean is new AWA.Wikis.Models.Wiki_Page_Info_Bean with record
      Module         : Modules.Wiki_Module_Access := null;
      Page           : Wiki_View_Bean_Access;

      --  List of words contained in the wiki page.
      Words          : aliased AWA.Tags.Beans.Tag_Info_List_Bean;
      Words_Bean     : AWA.Tags.Beans.Tag_Info_List_Bean_Access;

      --  List of wiki page links used in the wiki page.
      Links          : aliased AWA.Tags.Beans.Tag_Info_List_Bean;
      Links_Bean     : AWA.Tags.Beans.Tag_Info_List_Bean_Access;

      --  List of external links used in the wiki page.
      Ext_Links      : aliased AWA.Tags.Beans.Tag_Info_List_Bean;
      Ext_Links_Bean : AWA.Tags.Beans.Tag_Info_List_Bean_Access;
   end record;
   type Wiki_Page_Info_Bean_Access is access all Wiki_Page_Info_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_Page_Info_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Wiki_Page_Info_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   overriding
   procedure Load (Into    : in out Wiki_Page_Info_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Wiki_Page_Info_Bean bean instance.
   function Create_Wiki_Page_Info_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
                                        return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Wiki image info Bean
   --  ------------------------------
   --  The <tt>Wiki_Image_Info_Bean</tt> is used to provide information about a wiki image.
   type Wiki_Image_Info_Bean is new AWA.Wikis.Models.Wiki_Image_Bean with record
      Module         : Modules.Wiki_Module_Access := null;
      Page           : Wiki_View_Bean_Access;

      --  The folder name and image name.
      Folder_Name    : Ada.Strings.Unbounded.Unbounded_String;
      Name           : Ada.Strings.Unbounded.Unbounded_String;

      --  The wiki space identifier and wiki page identifer that uses the image.
      Wiki_Id        : ADO.Identifier := ADO.NO_IDENTIFIER;
      Page_Id        : ADO.Identifier := ADO.NO_IDENTIFIER;

      --  Information about images.
      List           : aliased AWA.Wikis.Models.Wiki_Image_Info_List_Bean;
      List_Bean      : AWA.Wikis.Models.Wiki_Image_Info_List_Bean_Access;
   end record;
   type Wiki_Image_Info_Bean_Access is access all Wiki_Image_Info_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Wiki_Image_Info_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Wiki_Image_Info_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Load the information about the image.
   overriding
   procedure Load (Into    : in out Wiki_Image_Info_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Wiki_Image_Info_BEan bean instance.
   function Create_Wiki_Image_Info_Bean (Module : in AWA.Wikis.Modules.Wiki_Module_Access)
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
