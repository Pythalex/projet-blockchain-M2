open Unix
open IPPortSet

(* network variables *)
let server_ip = inet_addr_loopback
let server_port = ref 0
let connect_to_ip = ref server_ip
let connect_to_port = ref 0

let set_connect_to_ip ip = connect_to_ip := inet_addr_of_string ip

let speclist = [
  ("-p", Arg.Set_int server_port, "Listening port number");
  ("--remote-ip", Arg.String set_connect_to_ip, "Remote miner's ip");
  ("--remote-port", Arg.Set_int connect_to_port, "Remote miner's port number");
]

let usage_msg = "Super bitcoin miner";;

Arg.parse speclist print_endline usage_msg;;

(* application variables *)
let miner_network = ref IPPortSet.empty


(** Connect to miner at given address and return miner network *)
let connect_to_miner self_ip self_port miner_ip miner_port =

  let addr = ADDR_INET(miner_ip, miner_port) in
  let s = socket PF_INET SOCK_STREAM  0 in
  connect s addr;

  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in

  Printf.printf "Connected to miner at %s:%d.\n%!" (string_of_inet_addr miner_ip) miner_port;

  (* self identification to remote miner *)
  output_value out_chan (self_ip, self_port);
  flush out_chan;

  Printf.printf "Sent identification (%s:%d).\n%!" (string_of_inet_addr self_ip) self_port;

  (* receive network map *)
  input_value in_chan


let greet_new_miner server_ip server_port in_chan out_chan =

  let (miner_ip, miner_port) = input_value in_chan in

  Printf.printf "Client at %s:%d\n%!" (string_of_inet_addr miner_ip) miner_port;

  output_value out_chan (IPPortSet.add (server_ip, server_port) !miner_network);
  flush out_chan;

  Printf.printf("Miner greeted.\n%!");

  (miner_ip, miner_port)


let main () =

  (* command argument check *)
  if !server_port = 0 then 
  begin
    Arg.usage speclist usage_msg;
    exit 0
  end;

  let addr = ADDR_INET(server_ip, !server_port) in
  let s = socket PF_INET SOCK_STREAM 0 in

  (* Call miner if a port was given in argument *)
  if !connect_to_port <> 0 then 
  begin
    miner_network := connect_to_miner server_ip !server_port 
      !connect_to_ip !connect_to_port;

    Printf.printf "Réponse reçue de la part du miner distant\n%!";
    printSet !miner_network;
  end;

  (* Option pour que la socket soit réutilisable *)
  setsockopt s SO_REUSEADDR true;

  bind s addr;

  Printf.printf "Listening on port %d...\n%!" !server_port;
  listen s 5;

  while true do
    let sc, _ = accept s in
    
    Format.printf "Un client se connecte@.";
    
    let in_chan = in_channel_of_descr sc in
    let out_chan = out_channel_of_descr sc in

    let miner_id = greet_new_miner server_ip !server_port in_chan out_chan in
    miner_network := IPPortSet.add miner_id !miner_network;
    printSet !miner_network;

    (* TODO : relayer l'id du nouveau mineur aux autres du réseau *)
  done 


let () = 
  main ()