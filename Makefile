.PHONY: build all run docs test

DOCS= \
	doc/prelude.md    \
	doc/dev.md        \
	doc/arch.md       \
	doc/network.md    \

# host sbcl
SBCL=sbcl --noinform

PANDOC_PDF_FLAGS=--pdf-engine=lualatex -V links-as-notes=true -H ./doc/cfg.tex
PANDOC_COMMON_FLAGS=--toc --toc-depth=2 --metadata title="leszcz" -f markdown+raw_tex --standalone

run:
	CL_SOURCE_REGISTRY=$(PWD) $(SBCL) \
		--eval "(ql:quickload :leszcz)" \
		--eval "(setf leszcz-constants:*prod* t)" \
		--eval "(leszcz:main)" \
		--quit
build: all
all: *.lisp
	mkdir -p build
	# rm -f *.fasl
	# $(SBCL) --load build.lisp --quit
	rm -f *.fasl
	wine sbcl.exe --noinform --core sbcl.core --load build.lisp --quit
	wine sbcl.exe --noinform --core sbcl.core --eval '(setf COMMON-LISP-USER::WINDBG t)' --load build.lisp --quit
test:
	# i don't think prove should be in an asdf package for leszcz, as we're dumping core to save the executeble and it would still be lingering in there
	CL_SOURCE_REGISTRY=$(PWD) $(SBCL) --dynamic-space-size 4096 \
		--eval "(ql:quickload :leszcz)" \
		--eval "(ql:quickload :prove)" \
		--load t/test.lisp \
		--quit
clean:
	rm -fr build leszcz.texi doc/*.html doc/*.pdf dist *.tgz
test-p2p:
	( \
	  CL_SOURCE_REGISTRY=$(PWD) $(SBCL) --eval "(ql:quickload :leszcz)" --eval "(leszcz::start-master-server)" --quit & \
	  sleep 1 ; \
	  CL_SOURCE_REGISTRY=$(PWD) $(SBCL) --eval "(ql:quickload :leszcz)" --eval "(leszcz::connect-to-master)" --quit & \
	  wait )
test-online-2:
	( \
	  ol -r server/leszcz-server.scm & \
	  CL_SOURCE_REGISTRY=$(PWD) $(SBCL) --eval "(ql:quickload :leszcz)" --eval "(leszcz::%online-host-menu)" --quit & \
	  sleep 1 ; \
	  CL_SOURCE_REGISTRY=$(PWD) $(SBCL) --eval "(ql:quickload :leszcz)" --eval "(leszcz::%online-join-menu)" --quit & \
	  wait )
docs:
	( for d in $(DOCS); do printf '\n\n\\newpage\n\n'; cat $$d ; done ) \
		| pandoc $(PANDOC_COMMON_FLAGS) $(PANDOC_PDF_FLAGS) -t pdf -o doc/leszcz.pdf


	CL_SOURCE_REGISTRY=$(PWD) $(SBCL) \
		--eval "(load (truename #P\"~/quicklisp/setup.lisp\"))" \
		--eval "(ql:quickload \"net.didierverna.declt\")" \
		--eval "(net.didierverna.declt:nickname-package)" \
		--eval "(declt:declt :leszcz :introspection-level 2 :license :bsd :declt-notice :short :tagline nil)" \
		--quit
	makeinfo --no-split --pdf leszcz.texi -o doc/leszcz-reference-manual.pdf
	makeinfo --no-split --html --css-include=doc/doc.css leszcz.texi -o doc/leszcz-reference-manual.html
pubcpy: dist docs
	rm -f leszcz-win64-dist.tgz leszcz-win64-dist.zip
	tar cvzf leszcz-win64-dist.tgz dist
	zip -r leszcz-win64-dist.zip dist
	yes | pubcpy leszcz-win64-dist.tgz
	yes | pubcpy leszcz-win64-dist.zip
	cd doc ; for f in leszcz-reference-manual.pdf leszcz-reference-manual.html leszcz.pdf ; do yes | pubcpy $$f ; done
dist: all docs
	mkdir -p dist/
	cp -v build/leszcz.exe dist/
	cp -v build/leszcz-debug.exe dist/
	cp -v libffi-8.dll dist/
	cp -v raylib5.5.dll dist/
	cp -v doc/*.pdf dist/
	cp -v README.dist dist/README.txt
	cp config.default.lisp dist/config.lisp
run-ecl:
	CL_SOURCE_REGISTRY=$(PWD) ecl \
		--eval "(ql:quickload :leszcz)" \
		--eval "(leszcz:main)" \
		--eval "(quit)"
build-ecl:
	CL_SOURCE_REGISTRY=$(PWD) ecl \
		--eval "(ext:install-c-compiler)" \
		--eval "(push #P\"$(PWD)/\" asdf:*central-registry*)" \
		--eval "(asdf:load-asd \"leszcz.asd\")" \
		--eval "(asdf:load-system :leszcz)" \
		--eval "(asdf:make-build :leszcz :type :program :monolithic t :move-here #P\"./\" :prologue-code '(progn (require 'asdf) (require 'sb-bsd-sockets)) :epilogue-code '(progn (leszcz:main) (ext:quit)))" \
		--eval "(quit)"
run-ccl:
	CL_SOURCE_REGISTRY=$(PWD) /home/kpm/soft/ccl/lx86cl64 \
		-e "(ql:quickload :leszcz)" \
		-e "(leszcz:main)" \
		-e "(quit)"

test-online:
	./t/test-online.sh
