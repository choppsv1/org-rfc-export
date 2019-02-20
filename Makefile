export PACKAGE_NAME := ox-rfc
export PROJ_DIR := $(shell pwd)

all:

test:
	$(MAKE) -C ert-tests test

clean:
	$(MAKE) -C ert-tests clean
	rm -f *~ *.elc

install:

