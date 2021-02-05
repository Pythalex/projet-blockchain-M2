# projet-blockchain-M2

## Compilation

Dans le terminal: `make`

## Lancer un miner

`./miner -p xxxx [--remote-ip "xxx.xxx.xxx.xxx" --remote-port xxxx]`

Exemple, pour le premier mineur:

`./miner -p 8000`

Le second:

`./miner -p 8001 --remote-ip "127.0.0.1" --remote-port 8000`

remarque: bien qu'on fournisse l'IP, le projet ne fonctionnera sans doute pas sur plusieurs machines car lors des communications, c'est cette
adresse privée qui est envoyée au lieu de l'adresse publique de la machine. Il faut que je change le type envoyé de inet_addr * int à sock_addr.

## Lancer un wallet

`./wallet -p xxxx --remote-ip "xxx.xxx.xxx.xxx" --remote-port xxxx`

Exemple:

`./wallet -p 8001 --remote-ip "127.0.0.1" --remote-port 8000`