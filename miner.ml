open Unix
open Commons
(* adresse IP locale 127.0.0.1 *)
type message =
  | Walletmsg of string
  | Hail of inet_addr * int
  | NetworkMap of InetSet.t
  | Broadcast of inet_addr * int
  | BroadcastWallet of string
let neighbors = ref InetSet.empty

let ip = inet_addr_of_string "127.0.0.1"
let port = ref 0
let connect_to_port = ref 0

let speclist = [
  ("-p", Arg.Set_int port, "Listening port number");
  ("--peer-port", Arg.Set_int connect_to_port, "Peer miner's port number");
]

let usage_msg = "Super bitcoin miner";;

Arg.parse speclist print_endline usage_msg;;
(* Adresse IP *)         
let addr = ADDR_INET(ip,!port)

(* création de la socket IPv4, TCP *)
let s = socket PF_INET SOCK_STREAM 0

(* Broadcast ip/port d'une nouvelle connexion *)
let broadcast neighbors (ip1,port1)  =
 (* InetSet.iter   *)
  let broadcast_l (ip,port) = 
   Printf.printf("sent new peer coordinates to (%s, %d)\n%!") (string_of_inet_addr ip) port ;
   let s = socket PF_INET SOCK_STREAM 0 in
   let addr_peer = ADDR_INET(ip,port) in
   connect s addr_peer;
   let out_chan = out_channel_of_descr s in
   output_value out_chan (Broadcast (ip1, port1));
   flush out_chan ;
   Unix.close s in
   InetSet.iter broadcast_l neighbors

(* Broadcast msg entrant d'un Wallet *)
let broadcast_wallet neighbors msg  =
 (* InetSet.iter   *)
  let broadcast_lw (ip,port) = 
   Printf.printf("peer was sent this msg :  %s\n%!") msg ;
   let s = socket PF_INET SOCK_STREAM 0 in
   let addr_peer = ADDR_INET(ip,port) in
   connect s addr_peer;
   let out_chan = out_channel_of_descr s in
   output_value out_chan (BroadcastWallet msg);
   flush out_chan ;
   Unix.close s in
   InetSet.iter broadcast_lw neighbors

(* Envoi de la liste du noeud *)
let send_list neighbors peer =
  let out_chan = out_channel_of_descr s in
  output_value out_chan neighbors;
  flush out_chan

  (* Connexion à un noeud, envoi ip/port et réception d'un set network *)
let connect_to_peer port1 = 
  let s = socket PF_INET SOCK_STREAM 0 in
  let addr_peer = ADDR_INET(ip,port1) in
  connect s addr_peer;
  let out_chan = out_channel_of_descr s in
  output_value out_chan (Hail (ip,!port));
  flush out_chan;
  neighbors :=  InetSet.add (ip,port1) !neighbors;
  let in_chan = in_channel_of_descr s in

  match input_value in_chan with
  | NetworkMap neighbors_received -> print_set neighbors_received;
    neighbors :=  InetSet.union neighbors_received !neighbors ;
  | _ -> ()
  


let () =
  (* Option pour que la socket soit réutilisable *)
  setsockopt s SO_REUSEADDR true;
  (* On branche la prise *)
  bind s addr;

  (* On est prêt à écouter, avec une file d'attente limitée à 5
     connexions en attente *)
  listen s 5;
  if !connect_to_port <> 0 then begin
  connect_to_peer !connect_to_port;
  end;

  while true do
    (* Le serveur se bloque en attendant une connexion d'un client *)
    let sc, _ = accept s in
    
    Format.printf "Un client se connecte";
    (* On crée deux canaux de communication (in et out) à partir de la
     s ocket : plus facile pour échanger des données *)
    let in_chan = in_channel_of_descr sc in
    let out_chan = out_channel_of_descr sc in

    try
      (* let pair = input_value in_chan in
      printPair pair;
      broadcast !neighbors pair; *)

      match input_value in_chan with
      | Hail (ip_l,port_l) -> printPair (ip_l,port_l);
        broadcast !neighbors (ip_l,port_l);
        output_value out_chan (NetworkMap !neighbors);
        neighbors := InetSet.add (ip_l,port_l) !neighbors;
        flush out_chan;
        (* Unix.close s; *)
      | Broadcast (ip_l,port_l) ->  Printf.printf("received new broadcast coords (%s, %d)\n%!") (string_of_inet_addr ip_l) port_l ;
         neighbors := InetSet.add (ip_l,port_l) !neighbors;
      | Walletmsg msg -> Printf.printf("Wallet says %s %!") msg;
        broadcast_wallet !neighbors msg;
      | BroadcastWallet msg -> Printf.printf("Some wallet says %s %!") msg;
      | _ -> ()
      
      
    with End_of_file -> ()
  done