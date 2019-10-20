FROM ubuntu:bionic

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

RUN apt-get update \
   && apt-get install -y --no-install-recommends make gcc \
            gnat gprbuild libaws3.3.2.2-dev \
            libsqlite3-dev libcurl4-openssl-dev git unzip \
            libgpr1-dev ca-certificates openssl man

WORKDIR /usr/src
