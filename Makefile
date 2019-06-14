# Prerequisites: stack, elm and uglifyjs binaries on PATH
.DEFAULT_GOAL := install

.PHONY: install
install: client minify server

.PHONY: server
server:
	stack install

.PHONY: client
client:
	cd client && elm make src/Main.elm --output=dist/js/elm.js --optimize && cd -

.PHONY: client-dev
client-dev:
	cd client && elm-live --dir dist --open -- src/Main.elm --output dist/js/elm.js

.PHONY: minify
minify: client
	cd client && uglifyjs dist/js/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=dist/js/elm.js && cd -

.PHONY: clean
clean:
	rm -rf client/elm-stuff
	stack clean
