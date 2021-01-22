all: miner IPPortSet

miner: miner.ml IPPortSet
	ocamlc -o miner unix.cma IPPortSet.cmo miner.ml

IPPortSet: IPPortSet.mli IPPortSet.ml
	ocamlc -c IPPortSet.mli IPPortSet.ml

clean:
	rm *.cmi *.cmo miner