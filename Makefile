DUNE:=dune

build:
	$(DUNE) build @install
#	$(DUNE) build test/test_main.exe

install:
	$(DUNE) install

clean:
	$(DUNE) clean


doc: FORCE
	$(DUNE) build @doc
	rm -rf /tmp/tjr_fs_shared
	cp -R _build/default/_doc/_html /tmp/tjr_fs_shared  # so we don't lose it on clean

view_doc:
	google-chrome  _build/default/_doc/_html/index.html


FORCE:
