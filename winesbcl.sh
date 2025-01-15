#!/bin/sh

SBCL="$HOME/.wine/drive_c/Program Files/Steel Bank Common Lisp/sbcl.exe"

if [ ! -f $SBCL ]; then
  SBCL=sbcl.exe
fi

`which wine` "$SBCL" $@
