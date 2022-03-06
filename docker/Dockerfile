FROM ciceron/ada-focal:latest

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

# We have to setup a timezone otherwise installation of tzdata will hang.
ENV TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update \
   && apt-get install -y sqlite3 unzip imagemagick pngcrush libjpeg-progs \
      yui-compressor liblzma-dev libfuse-dev pkg-config

RUN mkdir -p /usr/src \
   && cd /usr/src \
   && git clone https://github.com/stcarrez/ada-awa.git ada-awa \
   && cd ada-awa \
   && git submodule init \
   && git submodule update --recursive --remote \
   && ./configure --enable-ahven --disable-shared --prefix=/usr \
   && make -s \
   && make -s install

WORKDIR /usr/src/ada-awa
