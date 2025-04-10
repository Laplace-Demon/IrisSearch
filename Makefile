.PHONY: build test clean

build:
	@ dune build && dune install

test: build
	@ is $(INPUT)

clean:
	@ dune clean
