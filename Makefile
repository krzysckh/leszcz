.PHONY: build all

all:
	mkdir -p build
	sbcl --load build.lisp --quit
	wine sbcl.exe --core sbcl.core --load build.lisp --quit
clean:
	rm -fr build
