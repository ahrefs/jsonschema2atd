.PHONY: build test clean update

DUNE=opam exec -- dune

build:
	$(DUNE) build @all

install:
	opam install .

test:
	$(DUNE) build @runtest

clean:
	$(DUNE) clean

fmt:
# When running fmt make commands, if a dune build process is ongoing, dune
# will complain because of the lock file restriction. However, it should be ok
# to run these reformatting aliases without stopping the build, as they should
# not affect any of the ongoing build processes.
	-DUNE_CONFIG__GLOBAL_LOCK=disabled $(DUNE) build @fmt --auto-promote
