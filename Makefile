TMP_DOC_DIR:=/tmp/tjr_fs_shared

default: all

-include Makefile.ocaml

run_test:
	dune exec test/test_fs_shared.exe

# for auto-completion of Makefile target
clean::

