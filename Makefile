# Compiles, tests and packages i-iterate.el
# and bytecode compilation of *.el files

PACKAGE = i-iterate
DOCDST = ${PACKAGE}/docs
HTMLDOCDST = ${PACKAGE}/html-docs
DOCSRC = ${PACKAGE}/info
IC = makeinfo
ICO = --force
TEXI2HTML = texi2html
TEXI2HTMLO = --split section --use-nodes
HTML2WIKI = html2wiki
HTML2WIKIO = --dialect GoogleCode
TEXI = $(wildcard $(DOCSRC)/*.texi)
INFO = $(addprefix $(DOCDST)/,$(notdir $(TEXI:.texi=.info)))
WIKIDST = ../wiki
HTML = $(wildcard $(HTMLDOCDST)/*.html)
WIKI = $(addprefix $(WIKIDST)/,$(notdir $(HTML:.html=.wiki)))

$(DOCDST)/%.info: $(DOCSRC)/%.texi
	$(IC) $(ICO) -o $@ $<
	$(TEXI2HTML) $(TEXI2HTMLO) $<

$(WIKIDST)/%.wiki: $(HTMLDOCDST)/%.html
	$(HTML2WIKI) $(HTML2WIKIO) $< > $@

default: prepare $(INFO) move-html $(WIKI) byte-compile
	cp -r lisp info Makefile README i-pkg.el ${PACKAGE}

prepare:
	mkdir -p ${PACKAGE}
	mkdir -p ${DOCDST}
	mkdir -p ${HTMLDOCDST}

move-html:
	rm -f ${HTMLDOCDST}/*.html
	$(shell find ./ -maxdepth 1 -name "*.html" > /dev/null || \
mv -f *.html ${HTMLDOCDST}/)

byte-compile:
	emacs -Q -L ./lisp -batch -f batch-byte-compile ./lisp/*.el

clean:
	rm -f ./lisp/*.elc
	rm -f ./*.html
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
