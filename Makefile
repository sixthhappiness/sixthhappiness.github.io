htmls = all.html General.html index.html Jazz.html Programming.html T2.html

all: $(htmls) rss.xml artindexes

%.html : %.xml articles.xml
	xsltproc $< -o $@

rss.xml : rss-gen.xml articles.xml
	xsltproc rss-gen.xml -o rss.xml

ARTINDEXES := $(wildcard articles/*)

.PHONY : artindexes
artindexes :
	for dir in $(ARTINDEXES); do \
		$(MAKE) -C $$dir; \
	done

clean:
	rm -rf *~ $(htmls)
	for dir in $(ARTINDEXES); do \
		$(MAKE) clean -C $$dir; \
	done

upload: all
	rm -rf *~ articles/*/*~
	mirror akochoi
