.PHONY: all build doc test clean

all: build doc test

build:
	dune build

doc:
	dune build @doc

test:
	dune runtest -f --no-buffer -j 1

clean:
	dune clean
