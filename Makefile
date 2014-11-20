
a.out: spacestation.ml
	ocamlfind ocamlopt -package xml-light,str -linkpkg spacestation.ml

all: a.out
