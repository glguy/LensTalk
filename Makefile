PANDOC ?= pandoc
HIGHLIGHT_STYLE ?= zenburn

LensTalk.pdf: LensTalk.lhs
	$(PANDOC) -s --highlight-style=$(HIGHLIGHT_STYLE) -t beamer $^ -o $@

.PHONY: clean
clean:
	rm LensTalk.pdf
