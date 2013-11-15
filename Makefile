index.html: index.md template.html.tmpl
	pandoc --template=template.html.tmpl index.md > index.html
