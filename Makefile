OCB_FLAGS = -use-ocamlfind -use-menhir -I src -pkgs 'sedlex,ounit,core,menhirlib,ppx_deriving.show' -tags thread
OCB = ocamlbuild $(OCB_FLAGS)

build:native byte

run:build
	./main.native fixture.json

test:
	$(OCB) main_test.native
	./main_test.native

native:
	$(OCB) main.native

byte:
	$(OCB) main.byte

init:
	opam init -ya --comp=4.03.0
	eval `opam config env`

install:
	opam update
	opam install -y \
		ocp-indent \
		ocp-index \
		merlin \
		core \
		ocamlfind \
		sedlex \
		ppx_deriving\
		ounit \
		menhir
	opam user-setup install

clean:
	ocamlbuild -clean
