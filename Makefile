default: 
	$(MAKE) update_generated_doc
	$(MAKE) all

-include Makefile.ocaml

all::
#	$(DUNE) build bin/blk_store.exe
#	cp _build/default/bin/blk_store.exe .

update_generated_doc::
	cd src && (ocamldoc_pyexpander shared_ctxt.ml)
	cd src && (ocamldoc_pyexpander tjr_fs_shared.t.ml > tjr_fs_shared.ml)



run_test:
#	dune exec src-test/test_fs_shared.exe

# for auto-completion of Makefile target
clean::

