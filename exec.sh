#!/bin/sh
ocamlc -o commons unix.cma commons.ml
ocamlc -o client unix.cma client.ml
ocamlc  -thread unix.cma threads.cma -o server server.ml
ocamlc  -thread unix.cma threads.cma -o miner commons.cmo miner.ml
ocamlc -o wallet unix.cma wallet.ml

