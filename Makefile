all: common merkle miner wallet

miner: miner.ml common merkle
	ocamlfind ocamlopt -thread -o miner -linkpkg -package threads -package unix common.cmx miner.ml

wallet: wallet.ml common merkle
	ocamlfind ocamlopt -thread -o wallet -linkpkg -package threads -package unix common.cmx wallet.ml

common: common.ml
	ocamlfind ocamlopt -linkpkg -package unix common.ml

merkle: merkle.ml
	ocamlfind ocamlopt -linkpkg -package zarith merkle.ml

clean:
	rm *.cmi *.cmo *.cmx miner wallet