# grbr

**gr**-aph **br**-owser
A simple web app for traversing large directed graphs.

## Prerequisites

To build from source you need [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install), [elm](https://elm-lang.org/) and [uglifyjs](https://www.npmjs.com/package/uglify-js) binaries on PATH.
To run the tool you need [graphviz](https://www.graphviz.org/) and [elm-analyse](https://github.com/stil4m/elm-analyse) on PATH.

## Installation

Run `make` in the root of the project. This should install `grbr` binary on your PATH.

## Nix Installation

You can use nix to build and install this project including all its dependencies.
Note that `elm-analyse` and `graphviz` are also installed to the system along side main `grbr` binary.

```
$ git clone git@github.com:jhrcek/grbr.git
$ cd grbr
$ nix-env -i -f nix/release.nix
```

## Usage

Run `grbr` in a folder containing elm.json.
This should open a browser window showing the module dependencies of the project.
You can browse the module dependencies graph by clicking individual nodes in the graph.
