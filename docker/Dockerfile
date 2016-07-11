FROM debian:jessie

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

RUN apt-get update \
   && apt-get install -y make gcc gnat gprbuild libaws3.2.0-dev \
         libsqlite3-dev libcurl4-openssl-dev git unzip

RUN mkdir -p /usr/src \
   && cd /usr/src \
   && git clone https://github.com/stcarrez/ada-awa.git ada-awa \
   && cd ada-awa \
   && git submodule init \
   && git submodule update \
   && ./configure --enable-ahven --disable-shared --prefix=/usr \
   && make -s \
   && make -s install

WORKDIR /app
