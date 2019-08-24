# Ada Web Application

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/AWA.svg)](https://jenkins.vacs.fr/job/AWA/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/AWA.svg)](https://jenkins.vacs.fr/job/AWA/)
[![Documentation Status](https://readthedocs.org/projects/ada-awa/badge/?version=latest)](https://ada-awa.readthedocs.io/en/latest/?badge=latest)
[![Download](https://img.shields.io/badge/download-1.1.0-brightgreen.svg)](http://download.vacs.fr/ada-awa/awa-all-1.1.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-awa/1.1.0.svg)

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

# Using git

The AWA framework uses git submodules to integrate several other
projects.  To get all the sources, use the following commands:
```
   git clone git@github.com:stcarrez/ada-awa.git
   cd ada-awa
   git submodule init
   git submodule update
```

# Building AWA

If XML/Ada and AWS are already installed in your environment, configure,
build and install as follows:
```
   ./configure --prefix=/usr/local
   make
   make install
```

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

  * [Ada Web Application programmer's guide](https://ada-awa.readthedocs.io/en/latest/)
  * [Ada Database Objects Programmer's Guide](https://ada-ado.readthedocs.io/en/latest/)
  * [Ada Security Programmer's Guide](https://ada-security.readthedocs.io/en/latest/)
  * https://github.com/stcarrez/ada-awa/wiki/Documentation

# Tutorial

You may read the following tutorials to lean more about the technical details about
setting up and building an Ada Web Application:

  * Step 1: [Ada Web Application: Setting up the project](https://blog.vacs.fr/vacs/blogs/post.html?post=2014/05/08/Ada-Web-Application-Setting-up-the-project)
  * Step 2: [Ada Web Application: Building the UML model](https://blog.vacs.fr/vacs/blogs/post.html?post=2014/05/18/Ada-Web-Application--Building-the-UML-model)
  * Step 3: [Review Web Application: Creating a review](https://blog.vacs.fr/vacs/blogs/post.html?post=2014/06/14/Review-Web-Application-Creating-a-review)
  * Step 4: [Review Web Application: Listing the reviews](https://blog.vacs.fr/vacs/blogs/post.html?post=2014/07/19/Review-Web-Application-Listing-the-reviews)

# Presentations

  * [Secure Web Applications with AWA](https://fr.slideshare.net/StephaneCarrez1/secure-web-applications-with-awa) FOSDEM 2019

# Sites Using AWA

  * [Java 2 Ada](https://blog.vacs.fr/)
  * [Ada France](https://www.ada-france.org/adafr/index.html)
  * [Atlas](https://demo.vacs.fr/atlas/index.html)
  * [Jason](https://vdo/vdo/index.html)
