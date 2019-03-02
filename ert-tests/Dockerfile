FROM ubuntu:16.04
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y build-essential curl emacs24 tidy python-pip && \
    pip install pyang

WORKDIR /work
CMD [ "bash" ]
