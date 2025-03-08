#!/bin/sh
# -*- mode: sh; sh-basic-offset: 2 -*-

ol -r server/leszcz-server.scm &

CL_SOURCE_REGISTRY=$PWD \
  sbcl \
  --eval "(ql:quickload :leszcz)" \
  --eval "(in-package :leszcz)" \
  --load t/test-online.lisp \
  --quit

wait
