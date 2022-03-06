FROM ubuntu:focal

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

RUN apt-get update \
   && apt-get install -y --no-install-recommends make gcc \
            gnat gprbuild libaws19-dev \
            libsqlite3-dev libcurl4-openssl-dev git unzip \
            libgnatprj7-dev ca-certificates openssl man

WORKDIR /usr/src
