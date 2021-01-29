# projet-blockchain-M2

## Compilation

`make`

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

## Gestion du maillage (exercice 1)

Après connexion des trois miners, chacun obtient un ensemble des noeuds du réseau. Cette implémentation crée un graphe complet entre les miners,
les wallets ne reçoivent cependant pas les nouveaux noeuds. 

![proof](https://i.imgur.com/fDr9He6.png)
Exemple 1 : réseau complet avec que des miners

Sur l'exemple ci-dessous on connecte d'abord un wallet au premier miner. Lors de la connexion du second miner, le wallet ne reçoit pas de
notification de nouveau noeud.

![proof2](https://i.imgur.com/X0gJJuK.png)
Exemple 2 : réseau non complet avec wallet