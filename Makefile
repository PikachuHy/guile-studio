VERSION = 0.0.2

SOURCES = \
  guile-studio-configure.scm \
  logo.svg \
  Makefile \
  README.org

build:
	mkdir -p $(PREFIX)/bin && \
		mkdir -p $(PREFIX)/share && \
			guile -s guile-studio-configure.scm \
				$(PREFIX) $(EMACS_DIR) $(GUILE_DIR) $(PICT_DIR) $(ICONS_DIR) $(INPUTS)

dist:
	mkdir -p guile-studio && cp $(SOURCES) guile-studio && \
		tar -czf guile-studio-$(VERSION).tar.gz guile-studio
