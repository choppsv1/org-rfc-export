# Adapted from: git@github.com:choppsv1/ietf-docs.git
# Adapted from: git@github.com:rolandwalker/emacs-travis.git
EMACS=emacs

EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch -l "$(PWD)/ert-tests/setup-org.el"
TESTS=

#CURL=curl --silent
CURL=curl -fL --silent
WORK_DIR=$(shell pwd)
PACKAGE_NAME=ox-rfc
AUTOLOADS_FILE=$(PACKAGE_NAME)-autoloads.el
TRAVIS_FILE=.travis.yml
TEST_DIR=ert-tests
TEST_DEP_1=ert

ifdef ERT_LATEST
TEST_DEP_1_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=master
else
TEST_DEP_1_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=emacs-24.1
endif

.PHONY : build downloads downloads-latest autoloads test-autoloads test-travis \
         test test-interactive clean edit test-dep-1 test-dep-2 test-dep-3     \
         test-dep-4 test-dep-5 test-dep-6 test-dep-7 test-dep-8 test-dep-9

build :
	$(EMACS) $(EMACS_BATCH) --eval \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" ox-rfc.el

test-dep-1 :
	@cd $(TEST_DIR) && \
	$(EMACS) $(EMACS_BATCH) -L . -L .. -l $(TEST_DEP_1) || \
	(echo "Can't load test dependency $(TEST_DEP_1).el, run 'make downloads' to fetch it" ; exit 1)

$(TEST_DIR)/$(TEST_DEP_1).el:
	$(CURL) '$(TEST_DEP_1_URL)' > $@

downloads: $(TEST_DIR)/$(TEST_DEP_1).el

autoloads :
	$(EMACS) $(EMACS_BATCH) --eval                       \
	    "(progn                                          \
	      (setq generated-autoload-file \"$(WORK_DIR)/$(AUTOLOADS_FILE)\") \
	      (update-directory-autoloads \"$(WORK_DIR)\"))"

test-autoloads : autoloads
	@$(EMACS) $(EMACS_BATCH) -L . -l "./$(AUTOLOADS_FILE)"      || \
	 ( echo "failed to load autoloads: $(AUTOLOADS_FILE)" && false )

test-travis :
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi

test : build test-dep-1 test-autoloads
	@cd $(TEST_DIR)                                   && \
	(for test_lib in *-test.el; do                       \
	    $(EMACS) $(EMACS_BATCH) -L . -L .. -l cl -l $(TEST_DEP_1) -l $$test_lib --eval \
	    "(progn                                          \
	      (setq org-confirm-babel-evaluate nil)          \
	      (fset 'ert--print-backtrace 'ignore)           \
	      (ert-run-tests-batch-and-exit '(and \"$(TESTS)\" (not (tag :interactive)))))" || exit 1; \
	done)

clean :
	@rm -f $(AUTOLOADS_FILE) *.elc *~ */*.elc */*~ $(TEST_DIR)/$(TEST_DEP_1).el            \
	$(TEST_DIR)/draft-*.xml

docker:
	docker build -t test -f ert-tests/Dockerfile .

docker-run:
	docker run -v $(PWD):/work -it test
