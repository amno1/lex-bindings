#!/usr/bin/env bash

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi

$EMACS -batch -l lex-bindings.el -l dev/examples-to-docs.el -l dev/examples.el -f create-docs-file
