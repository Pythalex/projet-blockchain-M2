open Unix

(* adresse IP locale 127.0.0.1 *)
let ip = inet_addr_of_string "127.0.0.1"

let port = 8000

(* Adresse IP *)         
let addr = ADDR_INET(ip,port)

(* création de la socket IPv4, TCP *)
let s = socket PF_INET SOCK_STREAM 0

exception NotImplemented
      
let () =
  (* Option pour que la socket soit réutilisable *)
  setsockopt s SO_REUSEADDR true;

  (* On branche la prise *)
  bind s addr;

  (* On est prêt à écouter, avec une file d'attente limitée à 5
     connexions en attente *)
  listen s 5;

  while true do
    (* Le serveur se bloque en attendant une connexion d'un client *)
    let client_socket, client_addr = accept s in

    let client_ip, client_port = match client_addr with
      | ADDR_INET (ip, port) -> (ip, port)
      | ADDR_UNIX (str) -> raise NotImplemented
    in
    
    Format.printf "Un client se connected %s." (string_of_inet_addr client_ip);
    
    (* On crée deux canaux de communication (in et out) à partir de la
     s ocket : plus facile pour échanger des données *)
    let in_chan = in_channel_of_descr client_socket in
    let out_chan = out_channel_of_descr client_socket in

    try
      while true do
        (* On attend que le client nous envoie une valeur *)
        let m = input_line in_chan in
        
        (* Petite manipulation de la chaîne de caractères m reçue *)
        
        Format.printf "Msg reçu : %s@." m;
        
        let r = String.uppercase_ascii m in
        
        (* On envoie une réponse *)
        output_string out_chan (r^"\n");
        
        (* On oublie pas de vider le canal de sortie *)
        flush out_chan
      done
    with End_of_file -> ()
  done