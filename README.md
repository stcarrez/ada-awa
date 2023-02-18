# Ada Web Application

[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Bionic-AWA.svg)](https://jenkins.vacs.fr/job/Bionic-AWA/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Bionic-AWA.svg)](https://jenkins.vacs.fr/job/Bionic-AWA/)
[![codecov](https://codecov.io/gh/stcarrez/ada-awa/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-awa)
[![Documentation Status](https://readthedocs.org/projects/ada-awa/badge/?version=latest)](https://ada-awa.readthedocs.io/en/latest/?badge=latest)
[![Download](https://img.shields.io/badge/download-2.4.0-brightgreen.svg)](http://download.vacs.fr/ada-awa/awa-all-2.4.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-awa/2.4.0.svg)

Ada Web Application is a framework to build a Web Application in Ada 2012.
The framework provides several ready to use and extendable modules that are common
to many web application.  This includes the login, authentication, users, permissions,
managing comments, tags, votes, documents, images.  It provides a complete blog,
question and answers and a wiki module.

AWA simplifies the Web Application development by taking care of user management with
Google+, Facebook authentication and by providing the foundations on top of which you
can construct your own application.  AWA provides a powerful permission management
that gives flexibility to applications to grant access and protect your user's resources.

![AWA Features](https://github.com/stcarrez/ada-awa/wiki/images/awa-features.png)

AWA integrates the following projects:

* Ada Server Faces (https://github.com/stcarrez/ada-asf)
* Ada Servlet   (https://github.com/stcarrez/ada-servlet)
* Swagger Ada   (https://github.com/stcarrez/swagger-ada)
* ADO           (https://github.com/stcarrez/ada-ado)
* Ada Util      (https://github.com/stcarrez/ada-util)
* Ada Wiki      (https://github.com/stcarrez/ada-wiki)
* Ada EL        (https://github.com/stcarrez/ada-el)
* Ada Security  (https://github.com/stcarrez/ada-security)
* Ada Keystore  (https://github.com/stcarrez/ada-keystore)
* Ada LZMA      (https://github.com/stcarrez/ada-lzma)
* Dynamo        (https://github.com/stcarrez/dynamo)

These projects are distributed under the Apache License 2.0 or MIT license for Ada LZMA.

AWA relies on the following external projects:

* AWS      (https://libre.adacore.com/libre/tools/aws/)
* XMLAda   (https://libre.adacore.com/libre/tools/xmlada/)

These projects are provided as tarball in 'external' directory.
They are distributed under different licenses (GNU GPL).

The AWA framework integrates a set unit tests and provides a framework
to build unit tests for AWA applications.   The unit tests are based on
Ada Util test framework which itself is built on top of the excellent
Ahven test framework (Ahven sources is integrated in Ada Util).
You may get Ahven or Aunit at:

* Ahven    (http://ahven.stronglytyped.org/)
* AUnit    (https://libre.adacore.com/libre/tools/aunit/)

## Version 2.5.0 - Under development
  - Feature #22: Add command line support to register a new user
  - Feature #23: Add command line for database schema migration
  - Feature #25: Update database schema for ADO 2.4
  - Feature #26: Support to enable/disable user account
  - Feature #27: New option for the list command to print the last audit fields
  - Fix #30: Configuration to authenticate with GitHub is incomplete
  - Feature #31: Use <header>, <footer>, <main> for HTML layouts
  - Feature #32: Support several authentication methods for a user
  - Fix #33: Application name not recognized by some AWA commands
  - Fix #34: Configuration to authenticate with Gitlab is incomplete
  - Feature #35: Blog post creation improvement
  - Fix #37: Cannot use entity-permission for the url-policy
  - Update Trumbowyg editor to version 2.26.0
  - Use Markdown for the AWA comments and questions modules

## Version 2.4.0 - Aug 2022
- Add support for SQL queries embedded in applications with ARE
- Fix #20: Do not use git:// protocol
- New EasyMDE plugin to integrate the Easy Markdown Editor
- Update AWA blog and AWA wiki to use the EasyMDE editor for Markdown
- Use Dynamo 1.3.0, Ada Server Faces 1.5.0, Ada Servlet 1.6.0, OpenAPI Ada 0.6.0
- Use Ada Wiki 1.4.0, Ada Database Objects 2.3.0
- Use Ada Keystore 1.3.3, Ada EL 1.8.5, Ada Utility Library 2.5.0

[List all versions](https://github.com/stcarrez/ada-awa/blob/master/NEWS.md)

# Using git

The AWA framework uses git submodules to integrate several other
projects.  To get all the sources, use the following commands:
```
   git clone --recursive https://github.com/stcarrez/ada-awa.git
   cd ada-awa
```

# Development Host Installation

The PostgreSQL, MySQL and SQLite development headers and runtime are necessary
for building the Ada Database Objects driver.  The configure script will use
them to enable the ADO drivers. The configure script will fail if it does not
find any database driver.

## Ubuntu

First to get the LZMA and CURL support, it is necessary to install the following
packages before configuring AWA:

```
sudo apt-get install unzip liblzma-dev libcurl4-openssl-dev
```

MySQL Development installation:
```
sudo apt-get install libmysqlclient-dev
```

MariaDB Development installation:
```
sudo apt-get install mariadb-client libmariadb-client-lgpl-dev
```

SQLite Development installation:
```
sudo apt-get install sqlite3 libsqlite3-dev
```

PostgreSQL Development installation:
```
sudo apt-get install postgresql-client libpq-dev
```

## FreeBSD 12

First to get the LZMA, XML/Ada and CURL support, it is necessary
to install the following packages before configuring AWA:

```
pkg install lzma-18.05 curl-7.66.0 xmlada-17.0.0_1 aws-17.1_2
```

MariaDB Development installation:
```
pkg install mariadb104-client-10.4.7 mariadb104-server-10.4.7
```

SQLite Development installation:
```
pkg install sqlite3-3.29.0
```

PostgreSQL Development installation:
```
pkg install postgresql12-client-12.r1 postgresql12-server-12.r1
```

Once these packages are installed, you may have to setup the following
environment variables:
```
export PATH=/usr/local/gcc6-aux/bin:$PATH
export ADA_PROJECT_PATH=/usr/local/lib/gnat
```


## Windows

It is recommended to use msys2 available at https://www.msys2.org/
and use the `pacman` command to install the required packages.

```
pacman -S git
pacman -S make
pacman -S unzip
pacman -S base-devel --needed
pacman -S mingw-w64-x86_64-sqlite3
```

For Windows, the installation is a little bit more complex and manual.
You may either download the files from MySQL and SQLite download sites
or you may use the files provided by Ada Database Objects and
Ada LZMA in the `win32` directory.

For Windows 32-bit, extract the files:

```
cd ada-ado/win32 && unzip sqlite-dll-win32-x86-3290000.zip
cd ada-lzma/win32 && unzip liblzma-win32-x86-5.2.4.zip
```

For Windows 64-bit, extract the files:

```
cd ada-ado/win32 && unzip sqlite-dll-win64-x64-3290000.zip
cd ada-lzma/win32 && unzip liblzma-win64-x64-5.2.4.zip
```

If your GNAT 2019 compiler is installed in `C:/GNAT/2019`, you may
install the liblzma, MySQL and SQLite libraries by using msys cp with:

```
cp ada-lzma/win32/*.dll C:/GNAT/2021/bin
cp ada-lzma/win32/*.dll C:/GNAT/2021/lib
cp ada-lzma/win32/*.a C:/GNAT/2021/lib
cp ada-ado/win32/*.dll C:/GNAT/2021/bin
cp ada-ado/win32/*.dll C:/GNAT/2021/lib
cp ada-ado/win32/*.lib C:/GNAT/2021/lib
cp ada-ado/win32/*.a C:/GNAT/2021/lib
```

## Ada Web Server

The [Ada Web Server](https://libre.adacore.com/libre/tools/aws/) should be compiled with the
SSL support if you want to use the [OAuth 2.0](https://oauth.net/2/) protocol and integrate
with Google or Facebook authentication systems.  The AWS version shipped with GNAT 2021
will not work because it does not support SSL.

You may build AWS by using:

```
   git clone --recursive -b 20.2 https://github.com/AdaCore/aws
   cd aws
   make SOCKET=openssl setup build install
```

# Building AWA

The framework uses the `configure` script to detect the build environment.
In most cases you will configure, build and install with the following commands:
```
   ./configure --prefix=/usr/local
   make
   make install
```

# Next steps

Once you have built and installed the Ada Web Application framework,
you can use the `dynamo` command to create your first project by using
the `create-project` command:

```
dynamo create-project -l apache atlas your-email@domain.com
```

Have a look at the [Dynamo(1)](https://github.com/stcarrez/dynamo/wiki/Dynamo)
documentation for the detail of the `dynamo` command options.

Then, configure and build the project by using:

```
cd atlas
./configure
make generate build
```

At this step, you will get the binary server in `bin/atlas-server` and you can launch
it with:

```
./bin/atlas-server setup
```

The generated binary server provides several commands that you can use.
The list of commands and their options is described in:
[command (1)](https://github.com/stcarrez/ada-awa/blob/master/awa/docs/command.md).

Follow the Tutorial section and the
[Ada Web Application programmer's guide](https://ada-awa.readthedocs.io/en/latest/)
to proceed to the next development steps.

# Docker

A docker container is available for those who want to try AWA without installing
and building all required packages
(See [Ada Web Application on Docker](https://hub.docker.com/r/ciceron/ada-awa/).
To use the AWA docker container you can
run the following commands:

```
   sudo docker pull ciceron/ada-awa
```

# Documentation

The Ada Web Application programmer's guide describes how to setup the framework,
how you can setup and design your first web application with it,
and it provides detailed description of AWA components:

  * [Ada Web Application programmer's guide](https://ada-awa.readthedocs.io/en/latest/) [PDF](https://github.com/stcarrez/ada-awa/blob/master/awa/docs/awa-book.pdf)
  * [Ada Server Faces Programmer's Guide](https://ada-asf.readthedocs.io/en/latest/) [PDF](https://github.com/stcarrez/ada-asf/blob/master/docs/asf-book.pdf)
  * [Ada Database Objects Programmer's Guide](https://ada-ado.readthedocs.io/en/latest/) [PDF](https://github.com/stcarrez/ada-ado/blob/master/docs/ado-book.pdf)
  * [Ada Security Programmer's Guide](https://ada-security.readthedocs.io/en/latest/)
  * [Ada Wiki Programmer's Guide](https://ada-wiki.readthedocs.io/en/latest/) [PDF](https://github.com/stcarrez/ada-wiki/blob/master/docs/wiki-book.pdf)
  * [Ada Utility Library Programmer's Guide](https://ada-util.readthedocs.io/en/latest/)
  * https://github.com/stcarrez/ada-awa/wiki/Documentation

# Tutorial

You may read the following tutorials to lean more about the technical details about
setting up and building an Ada Web Application:

  * Step 1: [Ada Web Application: Setting up the project](https://blog.vacs.fr/vacs/blogs/post.html?post=2014/05/08/Ada-Web-Application-Setting-up-the-project)
  * Step 2: [Ada Web Application: Building the UML model](https://blog.vacs.fr/vacs/blogs/post.html?post=2014/05/18/Ada-Web-Application--Building-the-UML-model)
  * Step 3: [Review Web Application: Creating a review](https://blog.vacs.fr/vacs/blogs/post.html?post=2014/06/14/Review-Web-Application-Creating-a-review)
  * Step 4: [Review Web Application: Listing the reviews](https://blog.vacs.fr/vacs/blogs/post.html?post=2014/07/19/Review-Web-Application-Listing-the-reviews)

# Presentations

  * [Persistence with Ada Database Objects](https://fr.slideshare.net/StephaneCarrez1/persistence-with-ada-database-objects-ado) FOSDEM 2019
  * [Ada for Web Development](https://fr.slideshare.net/StephaneCarrez1/ada-techdays2019awa) AdaCore Tech Days 2019
  * [Secure Web Applications with AWA](https://fr.slideshare.net/StephaneCarrez1/secure-web-applications-with-awa) FOSDEM 2019
  * [Protect Sensitive Data with Ada Keystore](https://fr.slideshare.net/StephaneCarrez1/protect-sensitive-data-with-ada-keystore) Ada devroom FOSDEM 2020

# Articles

  * [Use Ada for Your Web Development | Electronic Design](https://www.electronicdesign.com/technologies/embedded-revolution/article/21119177/use-ada-for-your-web-development)

# Sites Using AWA

  * [Java 2 Ada](https://blog.vacs.fr/)
  * [Ada France](https://www.ada-france.org/adafr/index.html)
  * [Atlas](https://demo.vacs.fr/atlas/index.html)
  * [Jason Project Manager](https://vdo.vacs.fr/vdo/index.html)
  * [Porion Build Manager](https://porion.vacs.fr/porion/index.html)
