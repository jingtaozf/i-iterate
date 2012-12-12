# Compiles, tests and packages i-iterate.el
# and bytecode compilation of *.el files
# This script uses
# http://search.cpan.org/~martykube/HTML-WikiConverter-GoogleCode-0.12/
# for generating GoogleWiki pages (unfortunately, it doesn't capitalize
# the names of the pages).

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

default: prepare $(INFO) move-html $(WIKI) rename-wiki byte-compile
	cp -r lisp info Makefile README i-pkg.el ${PACKAGE}

prepare:
	mkdir -p ${PACKAGE}
	mkdir -p ${DOCDST}
	mkdir -p ${HTMLDOCDST}

move-html:
	$(shell [[ -e `find ./ -maxdepth 1 -name "*.html"` ]] || \
mv -f *.html ${HTMLDOCDST}/)

rename-wiki:
	$(shell cd ${WIKIDST} && rename 'i-iterate' 'Iterate' *.wiki)
	$(shell find ${WIKIDST} -name "*.wiki" -exec \
sed -i 's/\[i-iterate/\[Iterate/g' '{}' \;)
	$(shell find ${WIKIDST} -name "*.wiki" -exec \
sed -i 's/\.html\#/\#/g' '{}' \;)
	$(shell find ${WIKIDST} -name "*.wiki" -exec \
sed -i 's/&lt;/\</g' '{}' \;)
	$(shell find ${WIKIDST} -name "*.wiki" -exec \
sed -i 's/&gt;/\>/g' '{}' \;)

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
