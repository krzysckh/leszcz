#!/bin/sh

set -xe

SBCL="$HOME/.wine/drive_c/Program Files/Steel Bank Common Lisp/asbcl.exe"

if [ ! -f "$SBCL" ]; then
  SBCL="./sbcl.exe --core sbcl.core"
fi

exec `which wine` $SBCL $@
