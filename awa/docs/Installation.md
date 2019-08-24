# Installation

This chapter explains how to build and install the Ada Web Application framework.

## Before Building

Before building the framework, you will need:

* The [GNAT Ada compiler](http://libre.adacore.com/tools/gnat-gpl-edition/),
* Either the MySQL, PostgreSQL or SQLite development headers installed,
* [XML/Ada](http://libre.adacore.com/libre/tools/xmlada/),
* [Ada Web Server](http://libre.adacore.com/libre/tools/aws/).

First get, build and install the above tools and libraries.

The build process may also need the following commands:

* make (GNU make),
* gprbuild,
* gprinstall,
* unzip,
* sqlite3,
* mysql,
* psql,
* xsltproc

The Ada Web Application library also uses the following projects:

* [Ada Utility Library](https://github.com/stcarrez/ada-util),
* [Ada Expression Language Library](https://github.com/stcarrez/ada-el),
* [Ada Security Library](https://github.com/stcarrez/ada-security),
* [Ada Servlet Library](https://github.com/stcarrez/ada-servlet),
* [Ada Server Faces Library](https://github.com/stcarrez/ada-asf),
* [Ada Wiki Library](https://github.com/stcarrez/ada-wiki),
* [Ada Database Objects Library](https://github.com/stcarrez/ada-ado),
* [Swagger Ada Library](https://github.com/stcarrez/swagger-ada),
* [Dynamo](https://github.com/stcarrez/dynamo)

They are integrated as Git submodules.

## Development Host Installation

The PostgreSQL, MySQL and SQLite development headers and runtime are necessary
for building the Ada Database Objects driver.  The configure script will use
them to enable the ADO drivers. The configure script will fail if it does not
find any database driver.

### Ubuntu

First to get the LZMA and CURL support, it is necessary to install the following
packages before configuring AWA:

```
sudo apt-get install liblzma-dev libcurl4-openssl-dev
```

MySQL Development installation
```
sudo apt-get install libmysqlclient-dev
```

MariaDB Development installation
```
sudo apt-get install mariadb-client libmariadb-client-lgpl-dev
```

SQLite Development installation
```
sudo apt-get install libsqlite3-dev
```

PostgreSQL Development installation
```
sudo apt-get install postgresql-client libpq-dev
```

### Windows

For Windows, the installation is a little bit more complex and manual.
You may either download the files from MySQL and SQLite download sites
or you may use the files provided by Ada Database Objects in the `win32` directory.

It is recommended to use msys2 available at https://www.msys2.org/
and use the `pacman` command to install the required packages.

```
pacman -S git
pacman -S make
pacman -S unzip
pacman -S base-devel --needed
pacman -S mingw-w64-x86_64-sqlite3
```

If your GNAT 2019 compiler is installed in `C:/GNAT/2019`, you may
install the MySQL and SQLite libraries by using msys cp with:

```
cp ada-ado/win32/*.dll C:/GNAT/2019/bin
cp ada-ado/win32/*.dll C:/GNAT/2019/lib
cp ada-ado/win32/*.lib C:/GNAT/2019/lib
cp ada-ado/win32/*.a C:/GNAT/2019/lib
```

## Getting the sources

The AWA framework uses git submodules to integrate several other
projects.  To get all the sources, use the following commands:

```
   git clone git@github.com:stcarrez/ada-awa.git
   cd ada-awa
   git submodule init
   git submodule update
```

## Configuration

The library uses the `configure` script to detect the build environment,
check for [Ada Utility Library](https://github.com/stcarrez/ada-util) library.
The `configure` script provides several standard options
and you may use:

  * `--prefix=DIR` to control the installation directory,
  * `--enable-shared` to enable the build of shared libraries,
  * `--disable-static` to disable the build of static libraries,
  * `--enable-distrib` to build for a distribution and strip symbols,
  * `--disable-distrib` to build with debugging support,
  * `--enable-coverage` to build with code coverage support (`-fprofile-arcs -ftest-coverage`),
  * `--enable-aws` to build the AWS version provided by the AWA package,
  * `--enable-xmlada` to build the XML/Ada version provided by the AWA package,
  * `--enable-aws-secure-mail` to build with the support of AWS SMTPS,
  * `--with-aws=PATH` to control the installation path of Ada Web Server,
  * `--with-xmlada=PATH` to control the installation path of XML/Ada,
  * `--help` to get a detailed list of supported options.

In most cases you will configure with the following command:
```
./configure
```

By default, the framework will be installed in `/usr/local` directory.
If you want to install the framework in a specific directory, use the `--prefix` option as follows:

```
./configure --prefix=/opt/install-awa
```

## Build

After configuration is successful, you can build the library by running:
```
make
```


## Installation
The installation is done by running the `install` target:

```
make install
```

## Using

To use the library in an Ada project, add the following line at the beginning of your GNAT project file:

```
with "awa";
```

Depending on your application, you may also need to add the following GNAT projects which
are provided by one or several of the libraries that Ada Web Application relies on:

```
with "utilada";
with "elada";
with "security";
with "servletada";
with "servletada_aws";
with "asf";
with "ado_mysql";
with "ado_sqlite";
with "ado_postgresql";
```

The library comes with several optional modules that you decide to use according to your needs.
When you decide to use a module, you should add the GNAT project that corresponds to the module
you wish to integrate  For example, to use the `Jobs` and `Wikis` modules, you will need
the following lines in your GNAT project:

```
with "awa_jobs";
with "awa_wikis";
```


