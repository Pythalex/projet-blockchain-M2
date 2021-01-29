all: common miner wallet

miner: miner.ml common
	ocamlc -o miner unix.cma common.cmo miner.ml

wallet: wallet.ml common
	ocamlc -o wallet unix.cma common.cmo wallet.ml

common: common.ml
	ocamlc -c unix.cma common.ml

clean:
	rm *.cmi *.cmo miner wallet