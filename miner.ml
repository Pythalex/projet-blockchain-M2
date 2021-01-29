open Unix
open Common

(* network variables *)
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
  ]

let usage_msg = "Super bitcoin miner"

;;
Arg.parse speclist print_endline usage_msg

(* application variables *)
let miner_network = ref NodeSet.empty

let main () =
  (* command argument check *)
  if !server_port = 0 then (
    Arg.usage speclist usage_msg;
    exit 0 );

  let addr = ADDR_INET (server_ip, !server_port) in
  let s = socket PF_INET SOCK_STREAM 0 in

  (* Call miner if a port was given in argument *)
  if !connect_to_port <> 0 then (
    miner_network :=
      connect_to_miner Miner server_ip !server_port !connect_to_ip
        !connect_to_port;
    Printf.printf "Réponse reçue de la part du miner distant\n%!";
    printSet !miner_network );

  (* Option pour que la socket soit réutilisable *)
  setsockopt s SO_REUSEADDR true;

  (* Clean disconnect *)
  let disconnect_behavior signal =
    disconnect_from_network !miner_network server_ip !server_port
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle disconnect_behavior);

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

    ( match input_message with
    | Greetings (nodetype, ip, port) ->
        Printf.printf "Greetings new %s.\n%!" (nodetype_literal nodetype);
        greet_new_node !miner_network Miner server_ip !server_port out_chan;
        miner_network := NodeSet.add (nodetype, ip, port) !miner_network;

        print_endline "Sharing new node to rest of the network.";
        share_new_node !miner_network ip port
    | NetworkNewNode (nodetype, ip, port) ->
        Printf.printf "Received new node of type %s.\n%!"
          (nodetype_literal nodetype);
        miner_network := NodeSet.add (nodetype, ip, port) !miner_network
    | _ -> () );

    print_endline "New network:";
    printSet !miner_network
  done

let () = main ()
