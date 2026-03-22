FROM debian:trixie

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

RUN apt-get update \
   && apt-get install -y --no-install-recommends bash make gcc curl wget git openssl unzip libssl-dev libc6-dev \
   && apt-get install --reinstall ca-certificates \
   && update-ca-certificates

RUN cd /tmp \
    && wget -O /tmp/alr.zip https://github.com/alire-project/alire/releases/download/v2.1.0/alr-2.1.0-bin-x86_64-linux.zip \
    && unzip alr.zip \
    && mv bin/alr /usr/bin \
    && rm alr.zip

RUN /usr/bin/alr toolchain --select gnat_native \
    && /usr/bin/alr toolchain --select gprbuild

WORKDIR /usr/src
