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

## Gestion du maillage avec miner et wallet (exercice 1 et 2)

Après connexion des trois miners, chacun obtient un ensemble des noeuds du réseau. Cette implémentation crée un graphe complet entre les miners,
les wallets ne reçoivent cependant pas les nouveaux noeuds. 

![proof](https://i.imgur.com/fDr9He6.png)
Exemple 1 : réseau complet avec que des miners

Sur l'exemple ci-dessous on connecte d'abord un wallet au premier miner. Lors de la connexion du second miner, le wallet ne reçoit pas de
notification de nouveau noeud.

![proof2](https://i.imgur.com/X0gJJuK.png)
Exemple 2 : réseau non complet avec wallet

## Deconnexion

Les noeuds gèrent la deconnexion d'un noeud du réseau de façon silencieuse en le supprimant de leur ensemble.
Ils ne notifient pas tous leur voisins de cette deconnexion.

Remarque: Puisque pour le moment la seule communication est celle du maillage, il restera toujours au moins un noeud qui possède
dans son réseau le noeud déconnecté. Le noeud se fait supprimer du réseau après l'envoi de l'ensemble au nouveau noeud, qui possèdera
donc ce noeud mort.

## Exercice 3 - Threads

L'exercice trois est réalisé dans un fichier à part pour faciliter la reproduction de l'exercice.
Le fichier implémentant les deux mineurs concurrents est `parallel_mining_test.ml`.

Le fichier est compilé lors d'un `make`. Ou sinon précisément avec `make exercice3`.

Pour lancer le programme : `./parallel_mining_test.ml`. La difficulté est réglée dans la fonction main (pas de paramètre de programme).

![exemple](https://i.imgur.com/E0Ee8FH.png)

### Détails d'implémentation

Pour se faire passer le message, les threads utilisent une référence d'int commune représentant l'id courant sur lequel travaille le mineur le plus rapide.
Lors de chaque minage, les threads consultent cet id pour savoir si l'autre threads a réussi à trouver un hash satisfaisant. Lorsqu'un thread gagne,
il incrémente cet id commun et passe à la suite.
Compte tenu du caractère mono-coeur du programme, un thread qui gagne au premier coup va généralement gagner les autres manches. De plus, il s'agira souvent
du thread pair qui commence en premier, puisqu'il aura une longueur d'avance sur l'autre thread. Il arrive cependant que le second thread finisse par remporter une manche par le fruit du hasard.