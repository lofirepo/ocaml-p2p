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
- TAPS: Trust-Aware Peer Sampling
- TAC: Trust-Aware Clustering

## Building with Nix

Set up [Nix Flakes](https://nixos.wiki/wiki/Flakes), then run:

    nix build

## Developing with Nix

To get a development shell with all dependencies and build tools installed, run:

    nix develop

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
