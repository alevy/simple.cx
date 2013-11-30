PAGES := $(shell ls *.md)
PAGES_HTML := $(PAGES:%.md=out/%.html)

all: $(PAGES_HTML)

out/%.html: %.md template.html.tmpl
	pandoc --toc --toc-depth=3 --template=template.html.tmpl $< > $@

clean:
	rm $(PAGES_HTML)

