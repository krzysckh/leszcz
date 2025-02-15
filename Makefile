.PHONY: build all run docs test

DOCS= \
	doc/prelude.md    \
	doc/dev.md        \
	doc/arch.md       \
	doc/network.md    \

# host sbcl
SBCL=sbcl --noinform

PANDOC_PDF_FLAGS=--pdf-engine=lualatex -V links-as-notes=true -H ./doc/cfg.tex
PANDOC_COMMON_FLAGS=--toc --toc-depth=2 --metadata title="leszcz" -f markdown+raw_tex --standalone --bibliography=./doc/refs.bib --citeproc

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
	rm -fr build leszcz.texi doc/*.html doc/*.pdf
test-p2p:
	( \
	  CL_SOURCE_REGISTRY=$(PWD) $(SBCL) --eval "(ql:quickload :leszcz)" --eval "(leszcz::start-master-server)" --quit & \
	  sleep 1 ; \
	  CL_SOURCE_REGISTRY=$(PWD) $(SBCL) --eval "(ql:quickload :leszcz)" --eval "(leszcz::connect-to-master)" --quit & \
	  wait )
docs:
	( for d in $(DOCS); do printf '\n\n\\newpage\n\n'; cat $$d ; done ; printf '\n\n\\newpage\n\n# Odniesienia' ) \
		| pandoc $(PANDOC_COMMON_FLAGS) $(PANDOC_PDF_FLAGS) -t pdf -o doc/leszcz.pdf
	CL_SOURCE_REGISTRY=$(PWD) $(SBCL) \
		--eval "(ql:quickload :net.didierverna.declt)" \
		--eval "(net.didierverna.declt:nickname-package)" \
		--eval "(declt:declt :leszcz :introspection-level 2 :license :bsd :declt-notice :short :tagline nil)" \
		--quit
	makeinfo --no-split --pdf leszcz.texi -o doc/leszcz-reference-manual.pdf
	makeinfo --no-split --html --css-include=doc/doc.css leszcz.texi -o doc/leszcz-reference-manual.html
pubcpy: docs
	cd doc ; for f in leszcz-reference-manual.pdf leszcz-reference-manual.html leszcz.pdf ; do yes | pubcpy $$f ; done
