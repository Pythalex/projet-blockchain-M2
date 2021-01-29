all: common miner 

miner: miner.ml common
	ocamlc -o miner unix.cma common.cmo miner.ml

common: common.ml
	ocamlc -c unix.cma common.ml

clean:
	rm *.cmi *.cmo miner 