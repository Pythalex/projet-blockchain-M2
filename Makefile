all: common merkle miner wallet

miner: miner.ml common merkle
	ocamlfind ocamlopt -thread -o miner -linkpkg -package threads -package unix -package zarith -package hex merkle.cmx common.cmx miner.ml

wallet: wallet.ml common merkle
	ocamlfind ocamlopt -thread -o wallet -linkpkg -package threads -package unix -package zarith -package hex merkle.cmx common.cmx wallet.ml

common: common.ml merkle
	ocamlfind ocamlopt -linkpkg -package unix -package zarith -package hex merkle.cmx common.ml

merkle: merkle.ml
	ocamlfind ocamlopt -linkpkg -package zarith merkle.ml

merkle-test: merkle.ml
	ocamlfind ocamlopt -o merkle-test -linkpkg -package zarith merkle.ml

clean:
	rm *.cmi *.cmo *.cmx miner wallet *.out