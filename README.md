[![Build Status](https://travis-ci.org/p2pcollab/ocaml-p2p.svg?branch=master)](https://travis-ci.org/p2pcollab/ocaml-p2p)

# Gossip-based protocols for P2P collaboration

P2Pcollab is a collacection of composable libraries
implementing gossip-based protocols for P2P collaboration.

These libraries are distributed under the AGPL-3.0-only license.

## Modules

- PolderCast: P2P topic-based pub/sub
- RingCast: P2P hybrid dissemination
- VICINITY: P2P clustering & topology management
- CYCLON: Random Peer Sampling

## Installation

The libraries can be installed via `opam`:

    opam install p2p
    opam install p2p-cyclon
    opam install p2p-cyclon-lwt
    opam install p2p-vicinity
    opam install p2p-vicinity-lwt
    opam install p2p-ringcast
    opam install p2p-ringcast-lwt
    opam install p2p-poldercast
    opam install p2p-poldercast-lwt

## Building

To build from source, generate documentation, and run tests, use `dune`:

    dune build
    dune build @doc
    dune runtest -f -j1 --no-buffer

In addition, the following `Makefile` targets are available
 as a shorthand for the above:

    make
    make build
    make doc
    make test

## Documentation

The documentation and API reference is generated from the source interfaces.
It can be consulted [online][doc] or via `odig`, e.g.:

    odig doc p2p
    odig doc p2p-cyclon
    ...

[doc]: https://p2pcollab.net/doc/ocaml/


## License information

This project is available as open source under the terms of AGPL-3.0-only license. However, for accurate information, please check individual files. As well as for copyright information. 
