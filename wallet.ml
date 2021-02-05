open Unix
open Common

(* network variables *)
let name = ref ""

let server_ip = inet_addr_loopback

let server_port = ref 0

let connect_to_ip = ref server_ip

let connect_to_port = ref 0

let set_connect_to_ip ip = connect_to_ip := inet_addr_of_string ip

let speclist =
  [
    ("-p", Arg.Set_int server_port, "Listening port number");
    ("--remote-ip", Arg.String set_connect_to_ip, "Remote miner's ip");
    ("--remote-port", Arg.Set_int connect_to_port, "Remote miner's port number");
    ("-n", Arg.Set_string name, "Wallet id");
  ]

let usage_msg = "Super wallet"

;;
Arg.parse speclist print_endline usage_msg

(* application variables *)
let mynodetype = Wallet

let network = ref NodeSet.empty

let print_new_network network =
  print_endline "New network:";
  print_NodeSet network

let main () =
  (* command argument check *)
  if !server_port = 0 || !connect_to_port = 0 then (
    Arg.usage speclist usage_msg;
    exit 0 );

  let addr = ADDR_INET (server_ip, !server_port) in
  let s = socket PF_INET SOCK_STREAM 0 in

  (* Call miner if a port was given in argument *)
  if !connect_to_port <> 0 then (
    network :=
      connect_to_miner !name mynodetype server_ip !server_port !connect_to_ip
        !connect_to_port;
    Printf.printf "Réponse reçue de la part du miner distant\n%!";
    print_NodeSet !network );

  (* Option pour que la socket soit réutilisable *)
  setsockopt s SO_REUSEADDR true;

  bind s addr;

  Printf.printf "Listening on port %d...\n%!" !server_port;
  listen s 5;

  while true do
    let client_socket, client_addr = accept s in
    let client_ip, client_port = extract_ip_port_from_sockaddr client_addr in

    let in_chan = in_channel_of_descr client_socket in
    let out_chan = out_channel_of_descr client_socket in

    let input_message = input_value in_chan in
    Printf.printf "Received %s from %s:%d.\n%!"
      (message_literal input_message)
      (string_of_inet_addr client_ip)
      client_port;

    match input_message with
    | NetworkNewNode (name, nodetype, ip, port) ->
        Printf.printf "Received new node of type %s.\n%!"
          (nodetype_literal nodetype);
        network := NodeSet.add (name, nodetype, ip, port) !network;
        print_new_network !network
    | _ -> print_endline "Ignoring."
  done

let () = main ()
