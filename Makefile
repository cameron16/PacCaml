SHELL=/bin/bash

main:
	ocamlbuild types.byte
	ocamlbuild ai.byte
	ocamlbuild processor.byte
	ocamlbuild -pkgs graphics gui.byte
	ocamlbuild -pkgs graphics main.byte

play:
	ocamlbuild types.byte
	ocamlbuild ai.byte
	ocamlbuild processor.byte
	ocamlbuild -pkgs graphics gui.byte
	ocamlbuild -pkgs graphics main.byte
	utop


# test:
# 	ocamlbuild -pkgs oUnit,str,unix test.byte && ./test.byte
