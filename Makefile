PAGES := $(shell ls pages/*.md)
PAGES_HTML := $(PAGES:pages/%.md=out/%.html)

all: $(PAGES_HTML)

out/%.html: pages/%.md template.html.tmpl
	pandoc -S --toc --toc-depth=3 --template=template.html.tmpl $< > $@

clean:
	rm -f $(PAGES_HTML)
	rm -f simple.cx.tar.gz

dist: all
	tar czf simple.cx.tar.gz -C out .

