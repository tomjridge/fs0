TMP_DOC_DIR:=/tmp/fs0
scratch:=/tmp/l/github/scratch

default: all

-include Makefile.ocaml

run:
	time $(DUNE) exec main

# for auto-completion of Makefile target
clean::
