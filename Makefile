.PHONY: build test clean

build:
	@ dune build

test:
	@ dune exec main $(INPUT)

clean:
	@ dune clean
