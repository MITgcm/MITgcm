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
	scp -r $(ALL) mitgcm.org:/u/httpd/html/devel_HOWTO/

devel_HOWTO.ps:  devel_HOWTO.sgml
	db2ps -d ldp.dsl devel_HOWTO.sgml

devel_HOWTO.pdf:  devel_HOWTO.sgml
	db2pdf -d ldp.dsl devel_HOWTO.sgml

devel_HOWTO_multipage:  devel_HOWTO.sgml
	db2html -o devel_HOWTO_multipage -d ./ldp.dsl devel_HOWTO.sgml

devel_HOWTO_onepage:  devel_HOWTO.sgml
	db2html -u -d ./ldp.dsl devel_HOWTO.sgml
	-rm -rf devel_HOWTO_onepage
	-mv devel_HOWTO devel_HOWTO_onepage

clean:
	rm -rf $(ALL) devel_HOWTO.junk
