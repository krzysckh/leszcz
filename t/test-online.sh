#!/bin/sh
# -*- mode: sh; sh-basic-offset: 2 -*-

if [ "$1" = "win" ]; then
  wine ol.exe -r server/leszcz-server.scm &
else
  ol -r server/leszcz-server.scm &
fi

CL_SOURCE_REGISTRY=$PWD \
  sbcl \
  --eval "(ql:quickload :leszcz)" \
  --eval "(in-package :leszcz)" \
  --load t/test-online.lisp \
  --quit

wait
