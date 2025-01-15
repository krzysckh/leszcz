TARGET=build/leszcz
BUILDAPP=buildapp

.PHONY: build

all:
	# $(MAKE) build TARGET=build/leszcz
	$(MAKE) build BUILDAPP="buildapp --sbcl './winesbcl.sh'" TARGET=build/leszcz.exe

build: *.lisp
	mkdir -p build

	CL_SOURCE_REGISTRY=$(PWD) $(BUILDAPP) \
		--output $(TARGET) \
		--load ~/quicklisp/setup.lisp \
		--eval '(ql:quickload :leszcz)' \
		--entry leszcz::main

clean:
	rm -fr build
