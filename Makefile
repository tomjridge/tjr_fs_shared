TMP_DOC_DIR:=/tmp/tjr_fs_shared

default: all

-include Makefile.ocaml

all::
#	$(DUNE) build bin/blk_store.exe
#	cp _build/default/bin/blk_store.exe .

run_test:
#	dune exec src-test/test_fs_shared.exe

# for auto-completion of Makefile target
clean::

