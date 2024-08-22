Version 2.6.0   -
  - Use AWS 24.0 from Alire
  - Cleanup build environment to drop configure

Version 2.5.0   - Sep 2023
  - Feature #22: Add command line support to register a new user
  - Feature #23: Add command line for database schema migration
  - Feature #25: Update database schema for ADO 2.4
  - Feature #26: Support to enable/disable user account
  - Feature #27: New option for the list command to print the last audit fields
  - Feature #31: Use `<header>`, `<footer>`, `<main>` for HTML layouts
  - Feature #32: Support several authentication methods for a user
  - Feature #35: Blog post creation improvement
  - Fix #30: Configuration to authenticate with GitHub is incomplete
  - Fix #33: Application name not recognized by some AWA commands
  - Fix #34: Configuration to authenticate with Gitlab is incomplete
  - Fix #37: Cannot use entity-permission for the url-policy
  - Fix #38: Image module does not handle SVG correctly
  - Fix #45: AWA start command option --max-upload-size and --max-form-size don't accept a value
  - Update Trumbowyg editor to version 2.26.0
  - Use Markdown for the AWA comments and questions modules

Version 2.4.0   - Aug 2022
  - Add support for SQL queries embedded in applications with ARE
  - Fix #20: Do not use git:// protocol
  - New EasyMDE plugin to integrate the Easy Markdown Editor
  - Update AWA blog and AWA wiki to use the EasyMDE editor for Markdown
  - Use Dynamo 1.3.0, Ada Server Faces 1.5.0, Ada Servlet 1.6.0, OpenAPI Ada 0.6.0
  - Use Ada Wiki 1.4.0, Ada Database Objects 2.3.0
  - Use Ada Keystore 1.3.3, Ada EL 1.8.5, Ada Utility Library 2.5.0

Version 2.3.1   - Feb 2022
  - Fix #13: AWA help command prints the wrong title 'akt tool to store and ...'
  - Fix #14: Avoid using gnatprep for the configuration of mail factory
  - Fix #15: Compilation with debug mode sometimes fails due to incorrect GNAT config project
  - Fix #16: Allow the configuration of server max upload size and max form submission size
  - Fix #17: Error generated when a blog article makes a reference to a non existing image
  - Fix #18: AWA blog plugin fails to serve an image when it is stored in the storage directory

Version 2.3     - Jul 2021
  - Update Trumbowyg editor to version 2.23.0
  - Fix generation of og:image meta for blog articles written in Markdown
  - Fix wiki preview with latest xkhtmltoimage 0.12.6
  - Use Dynamo 1.2.2, Ada Server Faces 1.4.3, Ada Servlet 1.5.2, OpenAPI Ada 0.5.0
  - Use Ada Wiki 1.3.2, Ada Database Objects 2.2.0
  - Use Ada Keystore 1.3.2, Ada EL 1.8.3, Ada Utility Library 2.4.1

Version 2.2     - Feb 2021
  - Fixed the Markdown js editor configuration
  - Send an event when a blog post is published (allows customisation such as sending e-mails)
  - Use Dynamo 1.2.1, Ada Server Faces 1.4.2, Ada Servlet 1.5.1, OpenAPI Ada 0.4.0
  - Use Ada Security 1.4.0, Ada Wiki 1.3.1, Ada Database Objects 2.1.2
  - Use Ada Keystore 1.3.1, Ada EL 1.8.2, Ada Utility Library 2.4.0

Version 2.1     - Nov 2020
  - Update Trumbowyg editor to version 2.21.0
  - Fix compilation issues with GNAT 2020
  - Update mail UI component to attach external files
  - Improved setup for secure configuration with Ada Keystore
  - Use Dynamo 1.2.0, Ada Server Faces 1.4.1, Ada Servlet 1.5.0, OpenAPI Ada 0.3.0
  - Use Ada Security 1.3.1, Ada Wiki 1.3.0, Ada Database Objects 2.1.1
  - Use Ada Keystore 1.2.1, Ada EL 1.8.1, Ada Utility Library 2.3.0

Version 2.0     - May 2020
  - Refactoring of build process and installation
  - New audit manager for database auditing
  - Support for Postgresql
  - Improvements of images and storage plugins
  - Update Trumbowyg editor to version 2.18.0
  - Update Flot library to version 4.2.0
  - Support for commands to configure, start, stop the server
  - New mail UI component <mail:attachment> to send attachments

Version 1.1     - Jul 2018
  - New trumbowyg plugin for WYSIWYG Javascript editor
  - New setup plugin for AWA application setup
  - Moved the samples to a separate project
  - New wiki plugin to write online wiki-based documentation
  - New flotcharts plugin to integraph jQuery Flot to display various graphs
  - Improvement of configure, build and installation with gprinstall when available
  - New counter plugin to track wiki page and blog post reads
  - Moved the wiki engine to Ada Wiki library
  - Support to display images in blog post
  - New image selector for wiki and blog post editors
  - Add a programmer's guide

Version 1.0     - Jul 2014
  - New countries plugin to provide country/region/city data models
  - New settings plugin to control application user settings
  - New tags plugin to easily add tags in applications
  - New <awa:tagList> and <awa:tagCloud> components for tag display
  - Add tags to the question and blog plugins
  - Add comments to the blog post

Version 0.3     - Feb 2013
  - New jobs plugin to manage asynchronous jobs
  - New storage plugin to manage a storage space for application documents
  - New votes plugin to allow voting on items
  - New question plugin to provide a general purpose Q&A

Version 0.2     - May 2012
  - New blog module and wiki engine
  - New event framework with configurable action listeners
  - Persistent event queues for the event framework
  - New mail UI components <mail:message>, <mail:body>, <mail:subject>,
    <mail:to>, <mail:from>, <mail:cc>, <mail:bcc> to build, format and
    send an email
  - New Javascript plugin Markedit with jQuery Markedit (MIT License)

Version 0.1     - Sep 2011
  - Core implementation of AWA framework on top of ADO and ASF
  - Provide a Users module for managing application users
  - Provide a Permissions module to checking permissions
  - Provide general purpose modules comments and blogs

