ARG UVERSION
FROM ubuntu:$UVERSION
RUN apt-get update && \
        apt-get upgrade -y && \
        apt-get install -y build-essential curl tidy python-pip && \
        pip install pyang

RUN apt-get install -y gnutls-bin gnutls-dev libncurses-dev libxml2 libxml2-dev libxslt-dev  python-dev

ENV SHELL=/bin/bash
ARG EVERSION
RUN curl -O http://ftp.gnu.org/gnu/emacs/emacs-$EVERSION.tar.gz
RUN tar xf emacs-$EVERSION.tar.gz
WORKDIR emacs-$EVERSION
RUN env CANNOT_DUMP=yes ./configure
RUN make
RUN make install

WORKDIR /work
CMD [ "bash" ]
