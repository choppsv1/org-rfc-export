export PACKAGE_NAME := ox-rfc
export PROJ_DIR := $(shell pwd)
export

all:

test:
	$(MAKE) -C ert-tests test

clean:
	$(MAKE) -C ert-tests clean
	rm -f *~ *.elc

install:


docker-build:
	$(MAKE) -C ert-tests docker-build

docker-run:
	docker run -v $(PROJ_DIR):/work -it org-rfc-test
