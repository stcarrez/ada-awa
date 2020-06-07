# Installation

This chapter explains how to build and install the Ada Web Application framework.

## Before Building

Before building the framework, you will need:

* The [GNAT Ada compiler](https://libre.adacore.com/tools/gnat-gpl-edition/),
* Either the MySQL, PostgreSQL or SQLite development headers installed,
* [XML/Ada](https://libre.adacore.com/libre/tools/xmlada/),
* [Ada Web Server](https://libre.adacore.com/libre/tools/aws/).

First get, build and install the above tools and libraries.
For the best experience, it is necessary to have the SSL support in
[Ada Web Server](https://libre.adacore.com/libre/tools/aws/).
Indeed, the [OpenID Authentication 2.0](https://openid.net/specs/openid-authentication-2_0.html) can only be used
through HTTPS.

The build process may also need the following commands:

* make (GNU make),
* gprbuild,
* gprinstall,
* unzip,
* sqlite3,
* mysql,
* psql,
* xsltproc,
* liblzma libraries (used by Ada LZMA),
* CURL libraries (used by CURL support in Ada Utility Library)

The Ada Web Application library also uses the following projects:

* [Ada LZMA](https://github.com/stcarrez/ada-lzma),
* [Ada Utility Library](https://github.com/stcarrez/ada-util),
* [Ada Expression Language Library](https://github.com/stcarrez/ada-el),
* [Ada Security Library](https://github.com/stcarrez/ada-security),
* [Ada Servlet Library](https://github.com/stcarrez/ada-servlet),
* [Ada Server Faces Library](https://github.com/stcarrez/ada-asf),
* [Ada Wiki Library](https://github.com/stcarrez/ada-wiki),
* [Ada Database Objects Library](https://github.com/stcarrez/ada-ado),
* [Ada Keystore Library](https://github.com/stcarrez/ada-keystore),
* [OpenAPI Ada Library](https://github.com/stcarrez/swagger-ada),
* [Dynamo](https://github.com/stcarrez/dynamo)

They are integrated as Git submodules.

## Getting the sources

The AWA framework uses git submodules to integrate several other
projects.  To get all the sources, use the following commands:

```
   git clone --recursive git@github.com:stcarrez/ada-awa.git
   cd ada-awa
```

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

### FreeBSD 12

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


### Windows

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
cp ada-lzma/win32/*.dll C:/GNAT/2019/bin
cp ada-lzma/win32/*.dll C:/GNAT/2019/lib
cp ada-lzma/win32/*.a C:/GNAT/2019/lib
cp ada-ado/win32/*.dll C:/GNAT/2019/bin
cp ada-ado/win32/*.dll C:/GNAT/2019/lib
cp ada-ado/win32/*.lib C:/GNAT/2019/lib
cp ada-ado/win32/*.a C:/GNAT/2019/lib
```

## Ada Web Server

The [Ada Web Server](https://libre.adacore.com/libre/tools/aws/) should be compiled with the
SSL support if you want to use the [OAuth 2.0](https://oauth.net/2/) protocol and integrate
with Google or Facebook authentication systems.  The AWS version shipped with GNAT 2019
and GNAT 2020 will not work because it does not support SSL.

You may build AWS by using:

```
   git clone --recursive -b 20.2 https://github.com/AdaCore/aws
   cd aws
   make SOCKET=openssl setup build install
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


