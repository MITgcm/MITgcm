#
# $ Header:  $
#

ALL = \
  devel_HOWTO.ps \
  devel_HOWTO.pdf \
  devel_HOWTO_multipage \
  devel_HOWTO_onepage

all: $(ALL)

install: all
	./install_devel_HOWTO

devel_HOWTO.ps:  devel_HOWTO.sgml
	docbook2ps -d ldp.dsl devel_HOWTO.sgml

devel_HOWTO.pdf:  devel_HOWTO.sgml
	docbook2pdf -d ldp.dsl devel_HOWTO.sgml

devel_HOWTO_multipage:  devel_HOWTO.sgml
	docbook2html -o devel_HOWTO_multipage -d ./ldp.dsl devel_HOWTO.sgml
	( cd devel_HOWTO_multipage && cp -p t1.htm index.html )

devel_HOWTO_onepage:  devel_HOWTO.sgml
	docbook2html -u -o devel_HOWTO_onepage -d ./ldp.dsl devel_HOWTO.sgml
	( cd devel_HOWTO_onepage && mv devel_HOWTO.html index.html )

clean:
	rm -rf $(ALL)
