SRC := $(wildcard src/*.idr)
WEB := web/index.html web/lib.js

all: dist

balls.js: $(SRC)
	pack --cg javascript build

dist: balls.js
	mkdir -p dist
	cp $(WEB) dist
	cp build/exec/balls dist/balls.js
