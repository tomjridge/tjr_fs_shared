DUNE:=dune

build:
	$(DUNE) build @install
#	$(DUNE) build test/test_main.exe

install:
	$(DUNE) install

clean:
	$(DUNE) clean


docs: FORCE
	$(DUNE) build @doc
	rm -rf docs/tjr_fs_shared
	cp -R _build/default/_doc/_html/* docs

view_doc:
	google-chrome  _build/default/_doc/_html/index.html


FORCE:
