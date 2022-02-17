ARG UVERSION
FROM ubuntu:$UVERSION
ENV SHELL=/bin/bash
ARG EVERSION

RUN apt-get update && \
    apt-get upgrade -y && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        build-essential \
        curl \
        gnutls-bin \
        gnutls-dev \
        libncurses-dev \
        libxml2 \
        libxml2-dev \
        libxslt-dev \
        pkg-config \
        python3-dev \
        python3-pip \
        tidy \
    && pip install pyang

RUN curl -fL http://ftp.gnu.org/gnu/emacs/emacs-$EVERSION.tar.gz | tar -xzf - && \
    (cd emacs-$EVERSION && \
     env CANNOT_DUMP=yes ./configure && \
     make && \
     make install)

WORKDIR /work
CMD [ "bash" ]
