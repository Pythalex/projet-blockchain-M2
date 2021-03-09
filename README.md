# projet-blockchain-M2

## Requirements

These third party packages are required to run this project

- zarith

    `opam install zarith`

- hex

    `opam install hex`

## Compilation

Dans le terminal: `make`

## Lancer un miner

`./miner -p xxxx [--ri "xxx.xxx.xxx.xxx" --rp xxxx]`

Exemple, pour le premier mineur:

`./miner -p 8000`

Le second:

`./miner -p 8001 --ri "127.0.0.1" --rp 8000`


## Lancer un wallet

`./wallet -p xxxx -i "xxx.xxx.xxx.xxx"

Exemple:

`./wallet -p 8001 -i "127.0.0.1"
