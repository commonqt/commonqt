all: index.html

%.html: %.xml index.xsl
	xsltproc index.xsl $< >$@.tmp
	mv -f $@.tmp $@
	chmod -w $@
