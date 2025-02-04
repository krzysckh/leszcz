.PHONY: build all run docs test

DOCS= \
	doc/prelude.md    \
	doc/dev.md        \
	doc/arch.md       \
	doc/network.md    \

# host sbcl
SBCL=sbcl --noinform

PANDOC_HTML_FLAGS=-H doc/doc.css
PANDOC_PDF_FLAGS=--pdf-engine=lualatex -V links-as-notes=true -H ./doc/cfg.tex
PANDOC_COMMON_FLAGS=--toc --toc-depth=2 --metadata title="leszcz" -f markdown+raw_tex+raw_html --standalone

run:
	CL_SOURCE_REGISTRY=$(PWD) $(SBCL) \
		--eval "(ql:quickload :leszcz)" \
		--eval "(leszcz:main)" \
		--quit
build: all
all: *.lisp
	mkdir -p build
	# $(SBCL) --load build.lisp --quit
	wine sbcl.exe --core sbcl.core --load build.lisp --quit
test:
	# i don't think prove should be in an asdf package for leszcz, as we're dumping core to save the executeble and it would still be lingering in there
	CL_SOURCE_REGISTRY=$(PWD) $(SBCL) --dynamic-space-size 4096 \
		--eval "(ql:quickload :leszcz)" \
		--eval "(ql:quickload :prove)" \
		--load t/test.lisp \
		--quit
clean:
	rm -fr build
docs:
	cat $(DOCS) \
		| pandoc $(PANDOC_COMMON_FLAGS) $(PANDOC_HTML_FLAGS) -t html -o doc/leszcz.html

	( for d in $(DOCS); do printf '\n\n\\newpage\n\n'; cat $$d ; done ) \
		| pandoc $(PANDOC_COMMON_FLAGS) $(PANDOC_PDF_FLAGS) -t pdf -o doc/leszcz.pdf
