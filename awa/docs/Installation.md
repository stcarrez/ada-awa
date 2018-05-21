# Installation

This chapter explains how to build and install the Ada Web Application framework.

## Before Building

Before building the framework, you will need:

* The [GNAT Ada compiler](http://libre.adacore.com/tools/gnat-gpl-edition/),
* Either the MySQL or SQLite development headers installed,
* [XML/Ada](http://libre.adacore.com/libre/tools/xmlada/),
* [Ada Web Server](http://libre.adacore.com/libre/tools/aws/).

First get, build and install the above tools and libraries.

The build process may also need the following commands:

* make (GNU make),
* gprbuild,
* unzip,
* sqlite3,
* mysql,
* xsltproc

The Ada Web Application library also uses the following projects:

* [AdaYaml](https://github.com/stcarrez/AdaYaml),
* [Ada Utility Library](https://github.com/stcarrez/ada-util),
* [Ada Expression Language Library](https://github.com/stcarrez/ada-el),
* [Ada Security Library](https://github.com/stcarrez/ada-security),
* [Ada Servlet Library](https://github.com/stcarrez/ada-servlet),
* [Ada Server Faces Library](https://github.com/stcarrez/ada-asf),
* [Ada Wiki Library](https://github.com/stcarrez/ada-wiki),
* [Ada Database Objects Library](https://github.com/stcarrez/ada-ado),
* [Dynamo](https://github.com/stcarrez/dynamo)

They are integrated as Git submodules.

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
with "util";
with "el";
with "security";
with "servlet";
with "servlet_aws";
with "asf";
with "ado";
```

The library comes with several optional modules that you decide to use according to your needs.
When you decide to use a module, you should add the GNAT project that corresponds to the module
you wish to integrate  For example, to use the `Jobs` and `Wikis` modules, you will need
the following lines in your GNAT project:

```
with "awa_jobs";
with "awa_wikis";
```


