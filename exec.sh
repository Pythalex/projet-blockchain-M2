#!/bin/sh
ocamlc -o client unix.cma client.ml
ocamlc  -thread unix.cma threads.cma -o server server.ml
ocamlc  -thread unix.cma threads.cma -o miner miner.ml

