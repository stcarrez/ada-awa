# Installation

This chapter explains how to build and install the library.

## Before Building

Before building the library, you will need:

* Either the MySQL or SQLite development headers installed,
* [XML/Ada](http://libre.adacore.com/libre/tools/xmlada/),
* [Ada Web Server](http://libre.adacore.com/libre/tools/aws/).

First get, build and install the above libraries.

The Ada Web Application library also uses the following projects:

* [Ada Utility Library](https://github.com/stcarrez/ada-util),
* [Ada Expression Language Library](https://github.com/stcarrez/ada-el),
* [Ada Security Library](https://github.com/stcarrez/ada-security),
* [Ada Servlet Library](https://github.com/stcarrez/ada-servlet),
* [Ada Server Faces Library](https://github.com/stcarrez/ada-asf),
* [Ada Wiki Library](https://github.com/stcarrez/ada-wiki),
* [Ada Database Objects Library](https://github.com/stcarrez/ada-ado),
* [Dynamo](https://github.com/stcarrez/dynamo)


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
  * `--enable-aws` to build with the support of AWS,
  * `--enable-aws-secure-mail` to build with the support of AWS SMTPS,
  * `--with-ada-util=PATH` to control the installation path of Ada Utility Library,
  * `--with-ada-el=PATH` to control the installation path of Ada Expression Language Library,
  * `--with-ada-servlet=PATH` to control the installation path of Ada Servlet Library,
  * `--with-ada-security=PATH` to control the installation path of Ada Security Library,
  * `--with-ada-asf=PATH` to control the installation path of Ada Server Faces Library,
  * `--with-ada-wiki=PATH` to control the installation path of Ada Wiki Library,
  * `--with-ada-ado=PATH` to control the installation path of Ada Database Objects Library,
  * `--with-aws=PATH` to control the installation path of Ada Web Server,
  * `--help` to get a detailed list of supported options.

In most cases you will configure with the following command:
```
./configure
```

## Build

After configuration is successful, you can build the library by running:
```
make
```

After building, it is good practice to run the unit tests before installing the library.
The unit tests are built and executed using:
```
make test
```
And unit tests are executed by running the `bin/util_harness` test program.

## Installation
The installation is done by running the `install` target:

```
make install
```

If you want to install on a specific place, you can change the `prefix` and indicate the installation
direction as follows:

```
make install prefix=/opt
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


