all: common miner wallet exercice3

miner: miner.ml common
	ocamlc -I +threads -o miner unix.cma threads.cma common.cmo miner.ml

wallet: wallet.ml common
	ocamlc -I +threads -o wallet unix.cma threads.cma common.cmo wallet.ml

common: common.ml
	ocamlc -c unix.cma common.ml

exercice3 : parallel_mining_test.ml common
	ocamlc -I +threads -o parallel_mining_test unix.cma threads.cma common.cmo parallel_mining_test.ml

clean:
	rm *.cmi *.cmo miner wallet