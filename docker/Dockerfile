FROM ciceron/gnat:latest

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

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
