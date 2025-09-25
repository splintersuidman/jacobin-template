PREFIX		:= dist
BUILDDIR	:= dist

.PHONY		:= all install clean TextSlide TitleSlide

all: TextSlide TitleSlide

install:
	mkdir -p $(PREFIX)
	cp -r $(BUILDDIR) $(PREFIX)

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

TextSlide: $(BUILDDIR) $(BUILDDIR)/textslide.html $(BUILDDIR)/TextSlide.js $(BUILDDIR)/style.css $(BUILDDIR)/img
TitleSlide: $(BUILDDIR) $(BUILDDIR)/titleslide.html $(BUILDDIR)/TitleSlide.js $(BUILDDIR)/style.css $(BUILDDIR)/img

$(BUILDDIR)/%.js: src/Jacobin/%.purs $(BUILDDIR)
	spago bundle --minify --module Jacobin.$* --outfile $@

$(BUILDDIR)/%.html: html/%.html
	cp $< $@

$(BUILDDIR)/%.css: style/%.css
	cp $< $@

$(BUILDDIR)/img: img
	cp -r img $(BUILDDIR)

clean:
	rm -r $(BUILDDIR)
