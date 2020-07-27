default: 
	$(MAKE) all

-include Makefile.ocaml

all::
#	$(DUNE) build bin/blk_store.exe
#	cp _build/default/bin/blk_store.exe .
	$(DUNE) build bin/test_shared_freelist.exe

update_generated_doc::
	cd src && (ocamldoc_pyexpander shared_ctxt.ml)
	cd src && (ocamldoc_pyexpander shared_ctxt_summary.t.ml > shared_ctxt_summary.ml)
	cd src && (ocamldoc_pyexpander blk_intf.ml)
	cd src && (ocamldoc_pyexpander buf_ops.ml)	
	cd src && (ocamldoc_pyexpander shared_freelist.ml)	
	cd src && (ocamldoc_pyexpander summary.t.ml > summary.ml)



run_test:
	dune exec bin/test_shared_freelist.exe
#	dune exec src-test/test_fs_shared.exe

# for auto-completion of Makefile target
clean::
	rm -f src/GEN*

