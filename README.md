# projet-blockchain-M2

## Requirements

Ce programme a été écrit en ocaml 4.10.0.

Ces packages tiers sont requis pour compiler le programme.

- zarith

    `opam install zarith`

- hex

    `opam install hex`

## Compilation

Dans le terminal: `make`

## Lancer un miner

`./miner -p xxxx [--ri "xxx.xxx.xxx.xxx" --rp xxxx] [-d x]`

Où
- p est le port d'écoute du mineur
- ri est l'ip d'un mineur distant à interroger pour entrer dans le réseau
- rp est le port de ce mineur distant
- d permet de régler la difficulté du puzzle (défault = 3)

Exemple, pour le premier mineur:

`./miner -p 8000`

Le second:

`./miner -p 8001 --ri "127.0.0.1" --rp 8000`

Le mineur n'a pas de commande d'interaction.

## Lancer un wallet

`./wallet -p xxxx -n %s [-i "xxx.xxx.xxx.xxx]"

Où
- p est le port d'écoute du mineur distant
- i est l'adresse du mineur, par défaut est réglée sur localhost
- n est le nom du wallet, utilisé pour les transactions

Exemple:

`./wallet -p 8001 -i "127.0.0.1 -n SuperWallet"

### Commandes wallet

Ouvrir un wallet donne ce résultat:

![mainWallet](https://i.imgur.com/HN0ybgx.png)

La commande `help` réaffiche les commandes possibles.

#### Show peers (1)

La commande `1` permet d'afficher les noeuds du réseau (seulement les mineurs).

![showpeers](https://i.imgur.com/DSxxD97.png)

On voit les deux noeuds du réseau.

#### Send Transaction (2)

La commande `2` permet d'envoyer une transaction au mineur distant. Cette transaction sera partagée aux autres mineurs
et un nouveau bloc sera créé pour accueillir cette transaction.

![sendtrans1](https://i.imgur.com/hieakWr.png)

La transaction est envoyée au Mineur 8000 qui l'a partagé au mineur 8001. Les deux se mettent à miner.

Le destinataire de la transaction n'a pas besoin d'être un utilisateur réel (pas d'authentification).

#### Refresh blockchain (3)

Lorsque les mineurs minent des blocs, ils n'envoient pas les nouveaux blocs aux wallets. Un wallet reçoit la blockchain à sa première
connexion au mineur ou avec cette commande explicite:

![refresh](https://i.imgur.com/LhdUV6a.png)

Dans cet exemple, on a repris la transaction précédente. Le mineur 8001 avait trouvé le bloc en premier et l'a partagé au mineur 8000 qui l'a accepté.
Le wallet refresh la blockchain et reçoit les nouveaux headers (le wallet ne stocke pas les transactions).

#### Confirm transaction (4)

Pour confirmer qu'une transaction est contenu dans un bloc. L'utilisateur passe par la commande `4`.
Le logiciel demande à l'utilisateur le hash de la transaction à valider.

![conftrans1](https://i.imgur.com/31T6Grq.png)

L'utilisateur a demandé la preuve de la transaction de hash f7630f2637585a78d2943abbe313c99e.
Le mineur 8000 a trouvé cette transaction dans le bloc 1 et crée la preuve de merkle qu'il envoie au wallet.
Le wallet reçoit une preuve associée au bloc 1 qu'il possède. Il vérifie alors la preuve et affiche le succès de celle-ci.

Plusieurs situations peuvent arriver:
- Le hash donné par l'utilisateur est inconnu.
- Le bloc de la preuve n'est pas possédé par le wallet (blockchain pas à jour)
- La transaction est connue mais le bloc est en minage

##### Situation 1 : Le hash est inconnu

Dans ce cas le wallet est simplement averti:

![conftransinconnu](https://i.imgur.com/S2ZM6u7.png)


##### Situation 2 : Le bloc n'est pas possédé

Dans la commande de confirmation de transaction, le wallet refresh automatiquement sa blockchain de headers dans le cas où le bloc 
associé à une preuve est inexistant dans sa mémoire. L'opération est alors transparente pour l'utilisateur.


##### Situation 3 : La transaction est en attente

Si le wallet est trop rapide dans sa demande, alors il reçoit un message d'attente.

![conftroprapide](https://i.imgur.com/G2xAL1a.png)


## Annexe

Nous avions tenté de lancer le programme sur plusieurs machines séparées. La logique du programme est censée autoriser ceci.
Cependant nous n'avons pas réussi à faire fonctionner une communication TCP entre deux programmes Ocaml. En particulier
les messages sont bien transmis entre les machines, mais la communication est bloquée (comme un blocage pare-feu, mais a priori ceci était censé être autorisé).
Après de nombreux essais sans succès, il a été décidé de ne pas poursuivre. Le programme ne fonctionne donc qu'en local.