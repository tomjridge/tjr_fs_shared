default: 
	$(MAKE) all

-include Makefile.ocaml

all::
#	$(DUNE) build bin/blk_store.exe
#	cp _build/default/bin/blk_store.exe .

update_generated_doc::
	cd src && (ocamldoc_pyexpander shared_ctxt.ml)
	cd src && (ocamldoc_pyexpander shared_ctxt_summary.t.ml > shared_ctxt_summary.ml)



run_test:
#	dune exec src-test/test_fs_shared.exe

# for auto-completion of Makefile target
clean::

