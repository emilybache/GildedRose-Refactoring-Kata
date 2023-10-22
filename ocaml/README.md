## Gilded Rose Kata for OCaml

### Requirements

To run the project, the following package must be available on your computer:
- `opam` `>= 2.0`

### Installation

At the root of the _ocaml_ directory, execute:
```sh
opam switch create . --deps-only
eval $(opam env)
```

It will install all the required dependencies for the project to run.

### Running

This project relies on `dune`. To build it, run this command in your terminal:
```sh
dune exec gilded_rose
```

### Testing

The test suite is built with `Alcostest`. To launch the tests, just type:
```sh
dune runtest
```
