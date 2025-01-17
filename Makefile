.PHONY: build all run

run:
	CL_SOURCE_REGISTRY=$(PWD) sbcl --eval "(ql:quickload :leszcz)" --eval "(leszcz:main)" --quit
build: all
all: *.lisp
	mkdir -p build
	sbcl --load build.lisp --quit
	wine sbcl.exe --core sbcl.core --load build.lisp --quit
clean:
	rm -fr build
