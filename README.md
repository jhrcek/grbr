# grbr

**gr**-aph **br**-owser
A simple web app for traversing large directed graphs.

## Prerequisites

You need `stack`, `elm` and `uglifyjs` binaries on PATH to be able to build this from source

## Installation

Run `make` in the root of the project. This should install `grbr` binary on your path

## TODO

- [x] given FGL graph, render it using graphviz as svg image
- [x] Highlight the central node, when viewing node in its context
- [x] highlight app's modules when showing all modules
- [x] configurable transitive reduction
- [x] search modules based on name
- [ ] configure if only app's modules or all (including dep's) modules should be shown
- [ ] add a title to the diagram https://stackoverflow.com/questions/6450765/how-do-you-center-a-title-for-a-diagram-output-to-svg-using-dot
- [ ] generate Graph files into temp folder, probably deleted on shutdown
- [ ] explain why package x dependes on package y
- [ ] hide clustering params for package deps
- [ ] make tred param work for package deps
- [ ] make it configurable if we want to see only outgoing or only incoming edges
