# Adapted from: git@github.com:choppsv1/ietf-docs.git
# Adapted from: git@github.com:rolandwalker/emacs-travis.git
TEST_DIR := $(shell pwd)
PROJ_DIR ?= $(TEST_DIR)/..
AVERSION ?= 3.6

ORG_RELEASE ?= 8.3.6
ORG_DIR := /tmp/org-$(ORG_RELEASE)
ORG_URL := https://code.orgmode.org/bzg/org-mode/archive/release_$(ORG_RELEASE).tar.gz
ORG_EX := $(ORG_DIR)/lisp/ox.el

EMACS ?= emacs
EMACS_CLEAN ?= -Q
EMACS_BATCH := $(EMACS_CLEAN) --batch -L $(ORG_DIR)/lisp
EMACS_TEST_VERSIONS := 24.3 25.3 26.1

AUTOLOADS_FILE := $(TEST_DIR)/$(PACKAGE_NAME)-autoloads.el
CURL := curl -fL --silent
TRAVIS_FILE := $(PROJ_DIR)/.travis.yml

.PHONY: autoloads build clean test test-autoloads test-travis

$(ORG_EX):
	mkdir -p $(ORG_DIR)
	(cd $(ORG_DIR) && $(CURL) $(ORG_URL) | tar --strip-components=1 -xzf -)
	(cd $(ORG_DIR) && make autoloads lisp)


build: $(ORG_EX)
	$(EMACS) $(EMACS_BATCH) --eval \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" $(PROJ_DIR)/$(PACKAGE_NAME).el

autoloads:
	$(EMACS) $(EMACS_BATCH) --eval                       \
	    "(progn                                          \
	      (setq generated-autoload-file \"$(AUTOLOADS_FILE)\") \
	      (update-directory-autoloads \"$(PROJ_DIR)\"))"

test-autoloads : autoloads
	@$(EMACS) $(EMACS_BATCH) -L .. -l "$(AUTOLOADS_FILE)"      || \
	 ( echo "failed to load autoloads: $(AUTOLOADS_FILE)" && false )

test-travis:
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi

test: build test-autoloads
	$(EMACS) $(EMACS_BATCH) -L .. -l ert -l unit-test.el -f ert-run-tests-batch-and-exit
	$(EMACS) $(EMACS_BATCH) -L .. -l ert -l ox-rfc-test.el -f ert-run-tests-batch-and-exit

clean:
	@rm -rf $(ORG_DIR)
	@rm -f auto $(AUTOLOADS_FILE) *.elc $(TEST_DIR)/draft-*.xml

docker-build:
	docker build --build-arg=UVERSION=16.04 --build-arg=EVERSION=24.3 -t org-rfc-test:24.3 .
	docker build --build-arg=UVERSION=18.04 --build-arg=EVERSION=26.1 -t org-rfc-test:26.1 .
	docker build --build-arg=UVERSION=18.04 --build-arg=EVERSION=25.3 -t org-rfc-test:25.3 .
	docker tag org-rfc-test:26.1 org-rfc-test

docker-push:
	for f in $(EMACS_TEST_VERSIONS); do docker tag org-rfc-test:$$f choppsv1/org-rfc-test:$$f; done
	for f in $(EMACS_TEST_VERSIONS); do docker push choppsv1/org-rfc-test:$$f; done

docker-run:
	docker run -v $(PROJ_DIR):/work -it org-rfc-test
