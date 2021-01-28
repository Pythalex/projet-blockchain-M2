open Unix
open Thread
(* adresse IP locale 127.0.0.1 *)
module Inte = struct 
   type t = inet_addr 
   (* use Pervasives compare *)
   let compare = compare
 end

 module INEET = Set.Make(Inte)

let ip = inet_addr_of_string "127.0.0.1"

let ocamlisfuckingtrash = Array.length Sys.argv - 1;;
let port =
if ocamlisfuckingtrash <> 0 then int_of_string Sys.argv.(1) else 8000
(* Adresse IP *)         
let addr = ADDR_INET(ip,port)

(* création de la socket IPv4, TCP *)
let s = socket PF_INET SOCK_STREAM 0

(* module SI = Set.Make(struct type t = int let compare = compare end) *)

let connect_to_peer port = 
  let s = socket PF_INET SOCK_STREAM 0 in
  let addr = ADDR_INET(ip,port) in
  connect s addr;
  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in
  let m =  "WASAAAAAP BICONNEEEECT" in
  output_string out_chan (m^"\n");
  flush out_chan;
  let r = input_line in_chan in
  Format.printf "%s@." r

let () =
  (* Option pour que la socket soit réutilisable *)
  setsockopt s SO_REUSEADDR true;
  (* Printf.printf "Port : %i" port; *)
  (* On branche la prise *)
  bind s addr;

  (* On est prêt à écouter, avec une file d'attente limitée à 5
     connexions en attente *)
  listen s 5;
  if port <> 8000 then connect_to_peer 8000;
  while true do
    (* Le serveur se bloque en attendant une connexion d'un client *)
    let sc, _ = accept s in
    
    Format.printf "Un client se connecte@.";
    (* On crée deux canaux de communication (in et out) à partir de la
     s ocket : plus facile pour échanger des données *)
    let in_chan = in_channel_of_descr sc in
    let out_chan = out_channel_of_descr sc in

    try
      (* while true do
        (* On attend que le client nous envoie une valeur *)
        let m = input_line in_chan in
        
        (* Petite manipulation de la chaîne de caractères m reçue *)
        
        Format.printf "Msg reçu : %s@." m;
        
        let r = String.uppercase_ascii m in
        
        (* On envoie une réponse *)
        output_string out_chan (r^"\n");
        
        (* On oublie pas de vider le canal de sortie *)
        flush out_chan
      done *)
      let m = input_line in_chan in
      Format.printf "Msg reçu : %s@." m;
        
      let r = String.uppercase_ascii m in
        
      (* On envoie une réponse *)
      output_string out_chan (r^"\n");
        
      (* On oublie pas de vider le canal de sortie *)
      flush out_chan
    with End_of_file -> ()
  done