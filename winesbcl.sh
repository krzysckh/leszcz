#!/bin/sh

WIN_SBCL="$HOME/.wine/drive_c/Program Files/Steel Bank Common Lisp/sbcl.exe"

`which wine` "$WIN_SBCL" $@
