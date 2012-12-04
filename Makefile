# Compiles, tests and packages i-iterate.el
# and bytecode compilation of *.el files

PACKAGE = i-iterate
DOCDST = ${PACKAGE}/docs
DOCSRC = ${PACKAGE}/info
IC = makeinfo
ICO = --force
TEXI = $(wildcard $(DOCSRC)/*.texi)
INFO = $(addprefix $(DOCDST)/,$(notdir $(TEXI:.texi=.info)))

$(DOCDST)/%.info: $(DOCSRC)/%.texi
	$(IC) $(ICO) -o $@ $<

default: prepare byte-compile $(INFO)
	cp -r lisp info Makefile README i-pkg.el ${PACKAGE}

prepare:
	mkdir -p ${PACKAGE}
	mkdir -p ${DOCDST}

byte-compile:
	emacs -Q -L ./lisp -batch -f batch-byte-compile ./lisp/*.el

clean:
	rm -f ./lisp/*.elc
	rm -rf ${DOCDST}
	rm -rf ${PACKAGE}

# We don't have an install script yet
install:
	emacs -Q -L . -batch -l etc/install ${DIR}

tar.bz2: default
	tar cjf ${PACKAGE}.tar.bz2 ${PACKAGE}

zip: default
	zip -r ${PACKAGE}.zip ${PACKAGE}

package: tar.bz2 zip

test:
	emacs -batch -L ./lisp -l ert -l ./tests/i-test.el \
	-f ert-run-tests-batch-and-exit
