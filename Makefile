TARGET=build/leszcz
BUILDAPP=buildapp
WINE=wine

.PHONY: build all

all:
	# $(MAKE) build TARGET=build/leszcz
	$(WINE) sbcl.exe --core sbcl.core --load build.lisp --quit

# build: *.lisp
# 	mkdir -p build


# 	CL_SOURCE_REGISTRY=$(PWD) $(BUILDAPP) \
# 		--output $(TARGET) \
# 		--load ~/quicklisp/setup.lisp \
# 		--eval '(ql:quickload :leszcz)' \
# 		--entry leszcz::main

clean:
	rm -fr build
